
import struct
from abc import ABCMeta
from typing import Literal
from gnuradio_pmt.pmt_types import PST


class PMT_t(metaclass=ABCMeta):
    def to_bytes(self) -> bytes:
        raise NotImplementedError('PMT subclasses must implement to_bytes')


class PMT:

    # parse_handlers = {el.value:   for el in list(PST)}
    @staticmethod
    def parse(data: bytes):
        # PMT.
        ...
    class TRUE(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.TRUE.value)

        @staticmethod
        def parse(data: bytes) -> Literal[True]:
            if PST(data[0]) == PST.TRUE:
                return True
            raise TypeError(f'incorrect type! expected PST.TRUE got {PST(data[0])}')

    class FALSE(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.FALSE.value)

        @staticmethod
        def parse(data: bytes) -> Literal[False]:
            if PST(data[0]) == PST.FALSE:
                return False
            raise TypeError(f'incorrect type! expected PST.FALSE got {PST(data[0])}')

    class NULL(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.NULL.value)

        @staticmethod
        def parse(data: bytes) -> None:
            if PST(data[0]) == PST.NULL:
                return None
            raise TypeError(f'incorrect type! expected PST.NULL got {PST(data[0])}')

    class STRING(PMT_t):
        def __init__(self, value: str) -> None:
            if isinstance(value, str):
                self.val: str = value
            else:
                raise TypeError

        def to_bytes(self) -> bytes:
            return struct.pack('>BH', PST.SYMBOL.value, len(self.val)) + self.val.encode('utf-8')

        @staticmethod
        def parse(data: bytes) -> str:
            if PST(data[0]) == PST.SYMBOL:
                return data[struct.unpack('>H', data[1:3])[0]:].decode('utf-8')
            raise TypeError(f'incorrect type! expected PST.SYMBOL got {PST(data[0])}')

    class INT32(PMT_t):
        def __init__(self, value: int) -> None:
            self.val: int = value
            if value > 0xFFFFFFFF:
                raise ValueError('INT32 value too big')

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.INT32.value, self.val)

        @staticmethod
        def parse(data: bytes) -> int:
            if PST(data[0]) == PST.INT32:
                return struct.unpack('>I', data[1:5])[0]
            raise TypeError(f'incorrect type! expected PST.INT32 got {PST(data[0])}')

    class INT64(PMT_t):
        def __init__(self, value: int) -> None:
            self.val: int = value

        def to_bytes(self) -> bytes:
            return struct.pack('>BQ', PST.INT64.value, self.val)

        @staticmethod
        def parse(data: bytes) -> int:
            if PST(data[0]) == PST.INT64:
                return struct.unpack('>Q', data[1:5])[0]
            raise TypeError(f'incorrect type! expected PST.INT64 got {PST(data[0])}')

    class VECTOR(PMT_t):
        def __init__(self, values: list[PMT_t]) -> None:
            self.val: list[PMT_t] = values

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.VECTOR.value, len(self.val)) + b''.join([el.to_bytes() for el in self.val])


    class TUPLE(PMT_t):
        def __init__(self, values: tuple[PMT_t, ...]) -> None:
            self.val: tuple[PMT_t, ...] = values

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.TUPLE.value, len(self.val)) + b''.join([el.to_bytes() for el in self.val])

    class PAIR(PMT_t):
        def __init__(self, car: PMT_t, cdr: PMT_t) -> None:
            self.car: PMT_t = car
            self.cdr: PMT_t = cdr

        def to_bytes(self) -> bytes:
            return struct.pack('>B', PST.PAIR.value) + self.car.to_bytes() + self.cdr.to_bytes()

        @staticmethod
        def parse(data: bytes):
            if PST(data[0]) == PST.PAIR:
                raise NotImplementedError
            raise TypeError(f'incorrect type! expected PST.DOUBLE got {PST(data[0])}')

    class DICT(PMT_t):
        def __init__(self, value: dict[PMT_t, PMT_t]) -> None:
            self.val: dict[PMT_t, PMT_t] = value

        def to_bytes(self) -> bytes:
            return b''.join([struct.pack('>B', PST.DICT.value) + PMT.PAIR(key, val).to_bytes()
                            for key, val in self.val.items()]) + PMT.NULL.to_bytes()

    class DOUBLE(PMT_t):
        def __init__(self, value: float) -> None:
            self.val: float = value

        def to_bytes(self) -> bytes:
            return struct.pack('>Bd', PST.DOUBLE.value, self.val)

        @staticmethod
        def parse(data: bytes) -> float:
            if PST(data[0]) == PST.DOUBLE:
                return struct.unpack('>d', data[1:5])[0]
            raise TypeError(f'incorrect type! expected PST.DOUBLE got {PST(data[0])}')

    class COMPLEX(PMT_t):
        def __init__(self, value: complex) -> None:
            self.val: complex = value

        def to_bytes(self) -> bytes:
            return struct.pack('>Bdd', PST.COMPLEX.value, self.val.real, self.val.imag)

        @staticmethod
        def parse(data: bytes) -> complex:
            if PST(data[0]) == PST.COMPLEX:
                real, imag = struct.unpack('>dd', data[1:9])
                return complex(real, imag)
            raise TypeError(f'incorrect type! expected PST.COMPLEX got {PST(data[0])}')

# class PMT_TRUE(PMT):
#     @staticmethod
#     def to_bytes() -> bytes:
#         return struct.pack('>B', PST.TRUE.value)


# class PMT_FALSE(PMT):
#     @staticmethod
#     def to_bytes() -> bytes:
#         return struct.pack('>B', PST.FALSE.value)

# class PMT_NULL(PMT):
#     @staticmethod
#     def to_bytes() -> bytes:
#         return struct.pack('>B', PST.NULL.value)

# class PMT_STRING(PMT):
#     def __init__(self, value: str) -> None:
#         if isinstance(value, str):
#             self.val: str = value
#         else:
#             raise TypeError

#     def to_bytes(self) -> bytes:
#         return struct.pack('>BH', PST.SYMBOL.value, len(self.val)) + self.val.encode('utf-8')


# class PMT_INT32(PMT):
#     def __init__(self, value: int) -> None:
#         self.val: int = value
#         if value > 0xFFFFFFFF:
#             raise ValueError('PMT_INT32 value too big')

#     def to_bytes(self) -> bytes:
#         return struct.pack('>BI', PST.INT32.value, self.val)


# class PMT_INT64(PMT):
#     def __init__(self, value: int) -> None:
#         self.val: int = value

#     def to_bytes(self) -> bytes:
#         return struct.pack('>BI', PST.INT64.value, self.val)


# class PMT_VECTOR(PMT):
#     def __init__(self, values: list[PMT]) -> None:
#         self.val: list[PMT] = values

#     def to_bytes(self) -> bytes:
#         return struct.pack('>BI', PST.VECTOR.value, len(self.val)) + b''.join([el.to_bytes() for el in self.val])


# class PMT_TUPLE(PMT):
#     def __init__(self, values: tuple[PMT, ...]) -> None:
#         self.val: tuple[PMT, ...] = values

#     def to_bytes(self) -> bytes:
#         return struct.pack('>BI', PST.TUPLE.value, len(self.val)) + b''.join([el.to_bytes() for el in self.val])


# class PMT_PAIR(PMT):
#     def __init__(self, car: PMT, cdr: PMT) -> None:
#         self.car: PMT = car
#         self.cdr: PMT = cdr

#     def to_bytes(self) -> bytes:
#         return struct.pack('>B', PST.PAIR.value) + self.car.to_bytes() + self.cdr.to_bytes()


# class PMT_DICT(PMT):
#     def __init__(self, value: dict[PMT, PMT]) -> None:
#         self.val: dict[PMT, PMT] = value

#     def to_bytes(self) -> bytes:
#         return b''.join([struct.pack('>B', PST.DICT.value) + PMT_PAIR(key, val).to_bytes()
#                          for key, val in self.val.items()]) + PMT_NULL.to_bytes()


# class PMT_DOUBLE(PMT):
#     def __init__(self, value: float) -> None:
#         self.val: float = value

#     def to_bytes(self) -> bytes:
#         return struct.pack('>Bd', PST.DOUBLE.value, self.val)


# class PMT_COMPLEX(PMT):
#     def __init__(self, value: complex) -> None:
#         self.val: complex = value

#     def to_bytes(self) -> bytes:
#         return struct.pack('>Bdd', PST.COMPLEX.value, self.val.real, self.val.imag)


if __name__ == '__main__':
    # vec = PMT.VECTOR([PMT.INT32(123), PMT.DOUBLE(2.3)])
    # val = PMT.DICT({PMT.STRING('freq'): PMT.INT32(437000000),
    #                 PMT.STRING('cr'): PMT.INT32(1),
    #                 PMT.STRING('bw'): PMT.INT32(125000),
    #                 PMT.DICT({PMT.STRING('b'): PMT.PAIR(PMT.INT32(312), PMT.INT32(567))}): PMT.STRING('hello')})
    # print(val.to_bytes().hex(' ').upper())
    # print([el.value for el in list(PST)])
    print(PST(5))

