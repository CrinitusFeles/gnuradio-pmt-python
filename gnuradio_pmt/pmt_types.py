from enum import Enum


class PST(Enum):
    TRUE = 0x00
    FALSE = 0x01
    STRING = 0x02
    INT32 = 0x03
    DOUBLE = 0x04
    COMPLEX = 0x05
    NULL = 0x06
    PAIR = 0x07
    VECTOR = 0x08
    DICT = 0x09
    UNIFORM_VECTOR = 0x0a
    UINT64 = 0x0b
    TUPLE = 0x0c
    INT64 = 0x0d
    UVI_ENDIAN_MASK = 0x80
    UVI_SUBTYPE_MASK = 0x7f
    UVI_LITTLE_ENDIAN = 0x00
    UVI_BIG_ENDIAN = 0x80
    UVI_U8 = 0x00
    UVI_S8 = 0x01
    UVI_U16 = 0x02
    UVI_S16 = 0x03
    UVI_U32 = 0x04
    UVI_S32 = 0x05
    UVI_U64 = 0x06
    UVI_S64 = 0x07
    UVI_F32 = 0x08
    UVI_F64 = 0x09
    UVI_C32 = 0x0a
    UVI_C64 = 0x0b
    COMMENT = 0x3b
    COMMENT_END = 0x0a
