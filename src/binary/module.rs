use super::{
    opcode::Opcode,
    section::{Function, SectionCode},
    types::{FuncType, FunctionLocal, ValueType},
};

use crate::binary::instruction::Instruction;
use nom::{
    bytes::complete::{tag, take},
    multi::many0,
    number::complete::{le_u32, le_u8},
    sequence::pair,
    IResult,
};
use nom_leb128::leb128_u32;
use num_traits::FromPrimitive as _;

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub magic: String,
    pub version: u32,
    pub type_section: Option<Vec<FuncType>>,
    pub function_section: Option<Vec<u32>>,
    pub code_section: Option<Vec<Function>>,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            magic: "\0asm".to_string(),
            version: 1,
            type_section: None,
            function_section: None,
            code_section: None,
        }
    }
}

impl Module {
    pub fn new(input: &[u8]) -> anyhow::Result<Module> {
        let (_, module) =
            Module::decode(input).map_err(|e| anyhow::anyhow!("failed to parse wasm: {}", e))?;
        Ok(module)
    }
    /**
    - バイナリの先頭から4バイト受け取り、\0asmであれば残りの入力を受け取る
    - 残りのからさらに4バイト読み取って残りとu32に変換した値の入力を受け取る
    **/
    fn decode(input: &[u8]) -> IResult<&[u8], Module> {
        let (input, _) = tag(b"\0asm")(input)?;
        let (input, version) = le_u32(input)?;
        let mut module = Module {
            magic: "\0asm".into(),
            version,
            ..Default::default()
        };
        // todo 再帰型で書き直す
        let mut remaining = input;
        while !remaining.is_empty() {
            match Self::decode_section_header(remaining) {
                Ok((input, (code, size))) => {
                    let (rest, section_contents) = take(size)(input)?;

                    match code {
                        SectionCode::Type => {
                            let (_, types) = decode_type_section(section_contents)?;
                            module.type_section = Some(types);
                        }
                        SectionCode::Function => {
                            let (_, func_idx_list) = decode_function_section(section_contents)?;
                            module.function_section = Some(func_idx_list);
                        }
                        SectionCode::Code => {
                            let (_, funcs) = decode_code_section(section_contents)?;
                            module.code_section = Some(funcs)
                        }
                        _ => todo!(),
                    };

                    remaining = rest;
                }
                Err(err) => return Err(err),
            }
        }
        Ok((input, module))
    }
    fn decode_section_header(input: &[u8]) -> IResult<&[u8], (SectionCode, u32)> {
        let (input, (code, size)) = pair(le_u8, leb128_u32)(input)?;
        Ok((
            input,
            (
                SectionCode::from_u8(code).expect("unexpected section code"),
                size,
            ),
        ))
    }
}

fn decode_value_type(input: &[u8]) -> IResult<&[u8], ValueType> {
    let (input, value_type) = le_u8(input)?;
    Ok((input, value_type.into()))
}

fn decode_type_section(input: &[u8]) -> IResult<&[u8], Vec<FuncType>> {
    let mut func_types = vec![];

    let (mut input, count) = leb128_u32(input)?;

    for _ in 0..count {
        let (rest, _) = le_u8(input)?;
        let mut func = FuncType::default();

        let (rest, size) = leb128_u32(rest)?;
        let (rest, types) = take(size)(rest)?;
        let (_, types) = many0(decode_value_type)(types)?;
        func.params = types;

        let (rest, size) = leb128_u32(rest)?;
        let (rest, types) = take(size)(rest)?;
        let (_, types) = many0(decode_value_type)(types)?;
        func.results = types;

        func_types.push(func);
        input = rest;
    }

    Ok((&[], func_types))
}

fn decode_function_section(input: &[u8]) -> IResult<&[u8], Vec<u32>> {
    let mut func_idx_list = vec![];
    let (mut input, count) = leb128_u32(input)?;

    for _ in 0..count {
        let (rest, idx) = leb128_u32(input)?;
        func_idx_list.push(idx);
        input = rest;
    }

    Ok((&[], func_idx_list))
}

fn decode_code_section(input: &[u8]) -> IResult<&[u8], Vec<Function>> {
    let mut functions = vec![];
    let (mut input, count) = leb128_u32(input)?;

    for _ in 0..count {
        let (rest, size) = leb128_u32(input)?;
        let (rest, body) = take(size)(rest)?;
        let (_, body) = decode_function_body(body)?;
        functions.push(body);
        input = rest;
    }

    Ok((&[], functions))
}

fn decode_function_body(input: &[u8]) -> IResult<&[u8], Function> {
    let mut body = Function::default();

    let (mut input, count) = leb128_u32(input)?;

    for _ in 0..count {
        let (rest, type_count) = leb128_u32(input)?;
        let (rest, value_type) = le_u8(rest)?;
        body.locals.push(FunctionLocal {
            type_count,
            value_type: value_type.into(),
        });
        input = rest;
    }

    let mut remaining = input;

    while !remaining.is_empty() {
        let (rest, inst) = decode_instructions(remaining)?;
        body.code.push(inst);
        remaining = rest;
    }

    Ok((&[], body))
}

fn decode_instructions(input: &[u8]) -> IResult<&[u8], Instruction> {
    let (input, byte) = le_u8(input)?;
    let op = Opcode::from_u8(byte).unwrap_or_else(|| panic!("invalid opcode: {:X}", byte));
    let (rest, inst) = match op {
        Opcode::LocalGet => {
            let (rest, idx) = leb128_u32(input)?;
            (rest, Instruction::LocalGet(idx))
        }
        Opcode::I32Add => (input, Instruction::I32Add),
        Opcode::End => (input, Instruction::End),
    };
    Ok((rest, inst))
}
#[cfg(test)]
mod tests {
    use crate::binary::types::FunctionLocal;
    use crate::binary::{
        instruction::Instruction,
        module::Module,
        section::Function,
        types::{FuncType, ValueType},
    };
    use anyhow::Result;

    #[test]
    fn decode_simplest_module() -> Result<()> {
        // プリアンブルしか存在しないwasmバイナリを生成
        let wasm = wat::parse_str("(module)")?;
        // バイナリをデコードしてModule構造体を生成
        let module = Module::new(&wasm)?;
        // 生成したModule構造体が想定通りになっているかを比較
        assert_eq!(module, Module::default());
        Ok(())
    }
    #[test]
    fn decode_simplest_func() -> Result<()> {
        let wasm = wat::parse_str("(module (func))")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType::default()]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![],
                    code: vec![Instruction::End]
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }
    #[test]
    fn decode_func_param() -> Result<()> {
        let wasm = wat::parse_str("(module (func (param i32 i64)))")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType {
                    params: vec![ValueType::I32, ValueType::I64],
                    results: vec![],
                }]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![],
                    code: vec![Instruction::End],
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }

    #[test]
    fn decode_func_local() -> Result<()> {
        let wasm = wat::parse_file("src/fixtures/func_local.wat")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType::default()]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![
                        FunctionLocal {
                            type_count: 1,
                            value_type: ValueType::I32,
                        },
                        FunctionLocal {
                            type_count: 2,
                            value_type: ValueType::I64,
                        }
                    ],
                    code: vec![Instruction::End],
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }

    #[test]
    fn decode_funv_add() -> Result<()> {
        let wasm = wat::parse_file("src/fixtures/func_add.wat")?;
        let module = Module::new(&wasm)?;
        assert_eq!(
            module,
            Module {
                type_section: Some(vec![FuncType {
                    params: vec![ValueType::I32, ValueType::I32],
                    results: vec![ValueType::I32],
                }]),
                function_section: Some(vec![0]),
                code_section: Some(vec![Function {
                    locals: vec![],
                    code: vec![
                        Instruction::LocalGet(0),
                        Instruction::LocalGet(1),
                        Instruction::I32Add,
                        Instruction::End
                    ],
                }]),
                ..Default::default()
            }
        );
        Ok(())
    }
}
