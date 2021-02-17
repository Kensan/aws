# Some notes about usage / implementation

To build the demo:

$ make && ./srv

Then send a request to server (non secure):

[1]
  $ curl --http2 http://localhost:1234/toto

  => note that Chrome or Firefox are implementing http2 only
     over secure TLS connection.

  => with curl we can start testing more easily the implementation
     of HTTP/2 with upgrade protocol.

 [2]
   $ curl --http2-prior-knowledge -o out http://localhost:1234/toto

Or secure:

[3]
  $ curl --http2 --cacert=cert.pem https://localhost:1234/toto

  => will be dealt with later

-----------------------
[1] non secure requests

In this mode the headers sent are (displayed by srv CB routine):

 1 Host = localhost:1234
 2 User-Agent = curl/7.74.0
 3 Accept = */*
 4 Connection = Upgrade, HTTP2-Settings
 5 Upgrade = h2c
 6 HTTP2-Settings = AAMAAABkAAQCAAAAAAIAAAAA


-----------------------
[2] HTTP/2 - no upgrade as prior knowledge that the server supports it

-----------------------
[3] secure requests

???
