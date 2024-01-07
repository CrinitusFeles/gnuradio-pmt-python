

import struct

from gnuradio_pmt.pmt import PMT, PMT_t
from gnuradio_pmt.pmt_types import PST


def tags(tags: list[tuple[PMT.STRING, PMT_t]]) -> bytes:
    PMT_MAGIC = 0x5FF0
    PMT_VERSION = 0x01
    data: bytes = b''
    if len(tags) == 0:
        return struct.pack('<HBQQ', PMT_MAGIC, PMT_VERSION, 0, len(tags)) + data
    data = b''.join(struct.pack('>QBH', 0, PST.STRING, len(tag[0].val)) + tag[0].to_bytes() + tag[1].to_bytes()
                    for tag in tags)
    return struct.pack('<HBQQ', PMT_MAGIC, PMT_VERSION, 0, len(tags)) + data
