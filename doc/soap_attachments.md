# SOAP attachments #
The `soap` application supports the sending and receiving of SOAP attachments
in accordance with the [W3C standard](https://www.w3.org/TR/SOAP-attachments).

Attachments are included in the messages using MIME. Each attachment has
its own set of MIME headers (comparable to HTTP headers). On the Erlang
side (both client and server), MIME messages are represented as a list
`[{[Header], Body}]`, where each `Header` is a tuple `{Name, Value}`, and
`Name` and `Value` are strings and `Body` is a binary.

## Client side
To generate a client that supports the sending of attachments in the
service request, a special option must be passed to `erlang2wsdl`:
`{attachments, true}`. In this case the generated functions will have an
additional 4th argument to pass the attachments.

Any attachments that are received from the server will always be available
in the `soap_response` tuple, irrespective of the value for the
`attachments` option (except in the case of an HTTP error response,
which cannot contain an attachment).

## Server side
On the server side the attachments are made available via the `soap_req`.
Attachments that are received from the client can be inspected using
`soap_req:req_attachments/1`; attachments that should be sent to the server
can be added to the request using `soap_req:set_resp_attachments/2`.

