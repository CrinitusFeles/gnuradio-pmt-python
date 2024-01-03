
import struct
from abc import ABCMeta
from gnuradio_pmt.pmt_types import PST


class PMT_t(metaclass=ABCMeta):
    def to_bytes(self) -> bytes:
        raise NotImplementedError('PMT subclasses must implement to_bytes')

class PMT:
    class TRUE(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.TRUE.value)

    class FALSE(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.FALSE.value)

    class NULL(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.NULL.value)

    class STRING(PMT_t):
        def __init__(self, value: str) -> None:
            if isinstance(value, str):
                self.val: str = value
            else:
                raise TypeError

        def to_bytes(self) -> bytes:
            return struct.pack('>BH', PST.SYMBOL.value, len(self.val)) + self.val.encode('utf-8')

    class INT32(PMT_t):
        def __init__(self, value: int) -> None:
            self.val: int = value
            if value > 0xFFFFFFFF:
                raise ValueError('INT32 value too big')

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.INT32.value, self.val)

    class INT64(PMT_t):
        def __init__(self, value: int) -> None:
            self.val: int = value

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.INT64.value, self.val)

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

    class COMPLEX(PMT_t):
        def __init__(self, value: complex) -> None:
            self.val: complex = value

        def to_bytes(self) -> bytes:
            return struct.pack('>Bdd', PST.COMPLEX.value, self.val.real, self.val.imag)

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
    vec = PMT.VECTOR([PMT.INT32(123), PMT.DOUBLE(2.3)])
    val = PMT.DICT({PMT.STRING('freq'): PMT.INT32(437000000),
                    PMT.STRING('cr'): PMT.INT32(1),
                    PMT.STRING('bw'): PMT.INT32(125000),
                    PMT.DICT({PMT.STRING('b'): PMT.PAIR(PMT.INT32(312), PMT.INT32(567))}): PMT.STRING('hello')})
    print(val.to_bytes().hex(' ').upper())
