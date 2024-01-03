# gnuradio-pmt-python

Pure python gnuradio pmt converter. This project can be usefull when you need just to convert python objects to gnuradio's pmt (e.x. control gnuradio flowgraph over network using zmq blocks) without installing conda and a lot of c++ dependencies.

## Installation

```
poetry add git+https://github.com/CrinitusFeles/gnuradio-pmt-python.git
```

or

```
git install git+https://github.com/CrinitusFeles/gnuradio-pmt-python.git
```

## Using

### List of available types

* PMT.TRUE
* PMT.FALSE
* PMT.NULL
* PMT.INT32
* PMT.INT64
* PMT.DOUBLE
* PMT.COMPLEX
* PMT.STRING
* PMT.PAIR
* PMT.TUPLE
* PMT.VECTOR
* PMT.DICT

After creating `PMT` object to send it over network you need to convert it to bytes calling method `PMT.to_bytes()`

### Working with Gnuradio's ZMQ Message Sink

```python
import zmq
from gnuradio_pmt.pmt import PMT

def server(addr: str = '0.0.0.0:5502'):
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


if __name__ == '__main__':
    server()
```

### Working with Gnuradio's ZMQ Message Source

```python
def client(addr: str = '172.31.2.119:5501'):
    context = zmq.Context()

    print("Connecting to server")
    socket = context.socket(zmq.PULL)
    socket.connect(f"tcp://{addr}")
    try:
        while True:
            msg = socket.recv()
            print(f'got_msg {len(msg)}: ', msg)
            print(msg.hex(' ').upper())
    except KeyboardInterrupt:
        print('Shutting down...')

if __name__ == '__main__':
    client()
```
