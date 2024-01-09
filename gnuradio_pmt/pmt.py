
import struct
from abc import ABCMeta
from typing import Any, Literal
from gnuradio_pmt.pmt_types import PST


class PMT_t(metaclass=ABCMeta):
    def to_bytes(self) -> bytes:
        raise NotImplementedError('PMT subclasses must implement to_bytes')


class PMT:
    @staticmethod
    def parse(data: bytes) -> Any:
        return PMT._parse(data)[0]

    @staticmethod
    def _parse(data: bytes) -> tuple[Any, int]:
        return PMT.__dict__[PST(data[0]).name]._parse(data)

    class TRUE(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.TRUE.value)

        @staticmethod
        def _parse(data: bytes) -> tuple[Literal[True], int]:
            if PST(data[0]) == PST.TRUE:
                return True, 1
            raise TypeError(f'incorrect type! expected PST.TRUE got {PST(data[0])}')

    class FALSE(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.FALSE.value)

        @staticmethod
        def _parse(data: bytes) -> tuple[Literal[False], int]:
            if PST(data[0]) == PST.FALSE:
                return False, 1
            raise TypeError(f'incorrect type! expected PST.FALSE got {PST(data[0])}')

    class NULL(PMT_t):
        @staticmethod
        def to_bytes() -> bytes:
            return struct.pack('>B', PST.NULL.value)

        @staticmethod
        def _parse(data: bytes) -> tuple[None, int]:
            if PST(data[0]) == PST.NULL:
                return None, 1
            raise TypeError(f'incorrect type! expected PST.NULL got {PST(data[0])}')

    class STRING(PMT_t):
        def __init__(self, value: str) -> None:
            if isinstance(value, str):
                self.val: str = value
            else:
                raise TypeError

        def to_bytes(self) -> bytes:
            return struct.pack('>BH', PST.STRING.value, len(self.val)) + self.val.encode('utf-8')

        @staticmethod
        def _parse(data: bytes) -> tuple[str, int]:
            if PST(data[0]) == PST.STRING:
                str_len: int = struct.unpack('>H', data[1:3])[0]
                result: str = data[3:3+str_len].decode('utf-8')
                return result, len(result) + 3
            raise TypeError(f'incorrect type! expected PST.SYMBOL got {PST(data[0])}')

    class INT32(PMT_t):
        def __init__(self, value: int) -> None:
            self.val: int = value
            if value > 0xFFFFFFFF:
                raise ValueError('INT32 value too big')

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.INT32.value, self.val)

        @staticmethod
        def _parse(data: bytes) -> tuple[int, int]:
            if PST(data[0]) == PST.INT32:
                return struct.unpack('>I', data[1:5])[0], 5
            raise TypeError(f'incorrect type! expected PST.INT32 got {PST(data[0])}')

    class INT64(PMT_t):
        def __init__(self, value: int) -> None:
            self.val: int = value

        def to_bytes(self) -> bytes:
            return struct.pack('>BQ', PST.INT64.value, self.val)

        @staticmethod
        def _parse(data: bytes) -> tuple[int, int]:
            if PST(data[0]) == PST.INT64:
                return struct.unpack('>Q', data[1:5])[0], 9
            raise TypeError(f'incorrect type! expected PST.INT64 got {PST(data[0])}')

    class VECTOR(PMT_t):
        def __init__(self, values: list[PMT_t]) -> None:
            self.val: list[PMT_t] = values

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.VECTOR.value, len(self.val)) + b''.join([el.to_bytes() for el in self.val])

        @staticmethod
        def _parse(data: bytes) -> tuple[list[Any], int]:
            if PST(data[0]) == PST.VECTOR:
                vec_size: int = struct.unpack('>I', data[1:5])[0]
                vec_list: list = []
                parsed_size = 0
                while len(vec_list) < vec_size:
                    val, size = PMT._parse(data[5 + parsed_size:])
                    parsed_size += size
                    vec_list.append(val)
                return vec_list, parsed_size + 5

            raise TypeError(f'incorrect type! expected PST.DOUBLE got {PST(data[0])}')


    class TUPLE(PMT_t):
        def __init__(self, values: tuple[PMT_t, ...]) -> None:
            self.val: tuple[PMT_t, ...] = values

        def to_bytes(self) -> bytes:
            return struct.pack('>BI', PST.TUPLE.value, len(self.val)) + b''.join([el.to_bytes() for el in self.val])

        @staticmethod
        def _parse(data: bytes) -> tuple[tuple, int]:
            if PST(data[0]) == PST.TUPLE:
                vec_size: int = struct.unpack('>I', data[1:5])[0]
                vec_list: list = []
                parsed_size = 0
                while len(vec_list) < vec_size:
                    val, size = PMT._parse(data[5 + parsed_size:])
                    parsed_size += size
                    vec_list.append(val)
                return tuple(vec_list), parsed_size + 5

            raise TypeError(f'incorrect type! expected PST.DOUBLE got {PST(data[0])}')

    class PAIR(PMT_t):
        def __init__(self, car: PMT_t, cdr: PMT_t) -> None:
            self.car: PMT_t = car
            self.cdr: PMT_t = cdr

        def to_bytes(self) -> bytes:
            return struct.pack('>B', PST.PAIR.value) + self.car.to_bytes() + self.cdr.to_bytes()

        @staticmethod
        def _parse(data: bytes) -> tuple[tuple[Any, Any], int]:
            if PST(data[0]) == PST.PAIR:
                car, car_size = PMT._parse(data[1:])
                cdr, cdr_size = PMT._parse(data[1 + car_size:])
                return (car, cdr), car_size + cdr_size + 1
            raise TypeError(f'incorrect type! expected PST.PAIR got {PST(data[0])}')

    class DICT(PMT_t):
        def __init__(self, value: dict[PMT_t, PMT_t]) -> None:
            self.val: dict[PMT_t, PMT_t] = value

        def to_bytes(self) -> bytes:
            return b''.join([struct.pack('>B', PST.DICT.value) + PMT.PAIR(key, val).to_bytes()
                            for key, val in self.val.items()]) + PMT.NULL.to_bytes()

        @staticmethod
        def _parse(data: bytes) -> tuple[dict[Any, Any], int]:
            if PST(data[0]) == PST.DICT:
                parsed_len = 0
                result = []
                while (PST(data[parsed_len]) == PST.DICT):
                    val, size = PMT._parse(data[1 + parsed_len:])
                    parsed_len += size + 1
                    result.append(val)
                return {key: val for key, val in result}, parsed_len + 1
            raise TypeError(f'incorrect type! expected PST.DICT got {PST(data[0])}')

    class DOUBLE(PMT_t):
        def __init__(self, value: float) -> None:
            self.val: float = value

        def to_bytes(self) -> bytes:
            return struct.pack('>Bd', PST.DOUBLE.value, self.val)

        @staticmethod
        def _parse(data: bytes) -> tuple[float, int]:
            if PST(data[0]) == PST.DOUBLE:
                return struct.unpack('>d', data[1:9])[0], 9
            raise TypeError(f'incorrect type! expected PST.DOUBLE got {PST(data[0])}')

    class COMPLEX(PMT_t):
        def __init__(self, value: complex) -> None:
            self.val: complex = value

        def to_bytes(self) -> bytes:
            return struct.pack('>Bdd', PST.COMPLEX.value, self.val.real, self.val.imag)

        @staticmethod
        def _parse(data: bytes) -> tuple[complex, int]:
            if PST(data[0]) == PST.COMPLEX:
                real, imag = struct.unpack('>dd', data[1:17])
                return complex(real, imag), 17
            raise TypeError(f'incorrect type! expected PST.COMPLEX got {PST(data[0])}')


if __name__ == '__main__':
    var: bytes = PMT.PAIR(PMT.PAIR(PMT.FALSE(), PMT.NULL()),
                          PMT.PAIR(PMT.STRING('123'), PMT.INT32(866))).to_bytes()
    var2: bytes = PMT.TUPLE((PMT.STRING('hello'), PMT.INT32(23),
                             PMT.PAIR(PMT.PAIR(PMT.FALSE(), PMT.NULL()),
                                      PMT.PAIR(PMT.STRING('123'), PMT.INT32(866))))).to_bytes()
    var3: bytes = PMT.VECTOR([PMT.DICT({PMT.STRING('hello'): PMT.COMPLEX(complex(23, 99)),
                              PMT.STRING('world'): PMT.DOUBLE(23.88)})]).to_bytes()
    print(var.hex(' ').upper())
    print(PMT.parse(var3))

