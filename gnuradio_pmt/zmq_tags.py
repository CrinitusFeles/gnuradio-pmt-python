import struct

from gnuradio_pmt.pmt import PMT, PMT_t

PMT_MAGIC = 0x5FF0
PMT_VERSION = 0x01

def tags(tags: list[tuple[PMT.STRING, PMT_t]]) -> bytes:
    data: bytes = b''
    if len(tags) == 0:
        return struct.pack('<HBQQ', PMT_MAGIC, PMT_VERSION, 0, len(tags)) + data
    src_id = b'\x01'
    data = b''.join(bytes(8) + tag[0].to_bytes() + tag[1].to_bytes() + src_id for tag in tags)
    return struct.pack('<HBQQ', PMT_MAGIC, PMT_VERSION, 0, len(tags)) + data


def parse_tags(data: bytes) -> tuple[list, bytes]:
    if struct.unpack('<HB', data[:3]) != (PMT_MAGIC, PMT_VERSION):
        raise ValueError(f'Incorrect tags header! {data[:3].hex(" ").upper()}')
    pmt_magic, pmt_version, offset, tags_amount = struct.unpack('<HBQQ', data[:19])
    parsed_len = 19
    tags = []
    for _ in range(tags_amount):
        offset: int = struct.unpack('<Q', data[parsed_len:parsed_len + 8])[0]
        parsed_len += 8
        tag_name, size = PMT._parse(data[parsed_len:])
        parsed_len += size
        tag_value, size = PMT._parse(data[parsed_len:])
        parsed_len += size
        tags.append({tag_name: tag_value})
        src_id: int = data[parsed_len]
        parsed_len += 1
    payload: bytes = data[parsed_len:]
    return tags, payload

if __name__ == '__main__':
    # tag stream from gnuradio
    tag_stream: str = 'F0 5F 01 19 00 00 00 00 00 00 00 01 00 00 00 00 00 00 00 19 00 00 00 00 00 00 00 02 00 0A 66 '\
                      '72 61 6D 65 5F 69 6E 66 6F 09 07 02 00 09 63 72 63 5F 76 61 6C 69 64 00 09 07 02 00 03 65 72 '\
                      '72 03 00 00 00 00 09 07 02 00 09 6C 64 72 6F 5F 6D 6F 64 65 03 00 00 00 00 09 07 02 00 03 63 '\
                      '72 63 03 00 00 00 01 09 07 02 00 07 70 61 79 5F 6C 65 6E 03 00 00 00 05 09 07 02 00 02 63 72 '\
                      '03 00 00 00 01 06 01 68 65 6C 6C 6F'
    print(parse_tags(bytes.fromhex(tag_stream)))

    my_tag_stream: bytes = tags([(PMT.STRING('tag_name'), PMT.DICT({PMT.STRING('key'): PMT.INT32(123)})),
                                 (PMT.STRING('tag_name2'), PMT.DICT({PMT.STRING('key2'): PMT.DOUBLE(3.3)}))])
    print(my_tag_stream.hex(' ').upper())
    print(parse_tags(my_tag_stream))