#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct FuncType {
    pub params: Vec<ValueType>,
    pub results: Vec<ValueType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    I32, // 0x7F
    I64,
}

impl From<u8> for ValueType {
    fn from(value: u8) -> Self {
        match value {
            0x7F => Self::I32,
            0x7E => Self::I64,
            _ => panic!("Invalid value type: {:X}", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionLocal {
    pub type_count: u32,
    pub value_type: ValueType,
}