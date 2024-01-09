import zmq
from gnuradio_pmt.pmt import PMT
from gnuradio_pmt.zmq_tags import parse_tags


def server(addr: str = '0.0.0.0:5501') -> None:
    context = zmq.Context()
    print("Start server…")
    socket = context.socket(zmq.PUSH)
    socket.bind(f"tcp://{addr}")
    try:
        while True:
            in_data: str = input('> ')
            socket.send(PMT.STRING(in_data).to_bytes())
    except KeyboardInterrupt:
        print('Shutting down...')


def client(addr: str = '172.25.111.199:5502') -> None:
    context = zmq.Context()

    print("Connecting to server")
    socket = context.socket(zmq.PULL)
    socket.connect(f"tcp://{addr}")
    try:
        while True:
            msg = socket.recv()
            print(msg)
            print(parse_tags(msg))
            # print(f'got_msg with len {len(msg)}: ', PMT.parse(msg))
            # print(msg.hex(' ').upper())
    except KeyboardInterrupt:
        print('Shutting down...')


if __name__ == '__main__':
    client()
