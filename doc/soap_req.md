# soap_req Reference #

- [Introduction](#introduction)
- [Data Types](#data-types)
- [Callback functions](#callback-function-index)

## Introduction ##
While handling a soap request, the `soap` application maintains some
information about that request. An opaque data structure (`soap_req()`) is
used to store this information. The information can be accessed (and in some cases modified) using functions that are provided by the `soap_req` module.


## Data Types

#### soap_req()
This is an opaque data type, used by the `soap` application to keep track of the information related to a soap request. 

#### soap_handler_state()

`soap_handler_state() :: any().`

This is data that is passed by the `soap` application between the invocations of the
callback functions in the handler module. The handler module can set a an 
initial value in the `init` callback function, this information becomes
available and can be updated by subsequent callback functions. See the [SOAP
server tutorial](soap_server_tutorial.md#the-init-callback) for more information.


#### http_status_code()

```erlang
http_status_code() :: integer().
```

The HTTP status code that is used for the HTTP response. In principle the code is prescribed by the SOAP specification: 200 for a successful request, 500 for a SOAP fault, and various other other codes for specific protocol errors. 

#### http_header()

`http_header() :: {string(), string()}`

#### soap_attachment()

```erlang
attachment_body() :: binary().
soap_attachment() :: {[http_header()], attachment_body()}.
```


## Function Index
- [content_type/1](#content_type1)
- [http_body/1](#http_body1)
- [method/1](#method1)
- [mime_headers/1](#mime_headers1)
- [req_attachments/1](#req_attachments1)
- [server_req/1](#server_req1)
- [set_resp_attachments/2](#set_resp_attachments2)
- [set_resp_http_headers/2](#set_resp_http_headers2)
- [set_resp_status_code/2](#set_resp_status_code2)
- [set_server_req/2](#set_server_req2)
- [soap_action/1](#soap_action1)
- [soap_version/1](#soap_version1)


### content_type/1 ###

```
content_type(soap_req()) -> string().
```

Returns the value of the Content-Type HTTP header of the HTTP request.

### http_body/1 ###

```erlang
http_body(soap_req()) -> binary() | undefined.
```

Returns the body of the HTTP request.

### method/1 ###

```erlang
method(soap_req()) -> string().
```

Returns the method of the HTTP request, e.g. "POST", "GET".

### mime_headers/1 ###


```erlang
spec mime_headers(soap_req()) -> [http_header()].
```

In case a request was received with attachments, the headers of the MIME
part that contains the SOAP message are accessible via this function. 

### req_attachments/1 ###

```erlang
req_attachments(soap_req()) -> [soap_attachment()].
```

Any attachment that was sent with the request (using the SOAP Messages with
attachments standards, i.e. in a MIME encoded request) is available via
this function.


### server_req/1 ###

```erlang
server_req(soap_req()) -> any().
```

This provides access to the information that is used by the http server
that is being used. The actual type depends on the http server, as do the functions that must be used.

- In the case of cowboy it will be a `cowboy_req`. 
- In the case of mochiweb it will be a module that implements the
  `mochiweb_request` interface. 


### set_server_req/2 ###

```erlang
set_server_req(soap_req(), any()) -> soap_req().
```

Makes it possible to update the information that is used by the http server
that is being used. Whether or not this is relevant depends on the http
server. For example in the case of cowboy it is important to update the
`server_req`, because actions executed on the `cowboy_req` are relevant 
when the http response is created.

For example: the handler module that is used to handle a soap request can
get access to the http headers of the incoming request, and it can set the
http headers of the response. Assuming that the cowboy http server is used,
that could be done as follows.

```erlang
%% This is the `check_http_conformance` callback in a handler module. This service uses 
%% basic authentication, and this callback is used to check it.
-spec check_soap_conformance(soap:soap_req(), soap:soap_handler_state()) ->  
      soap:soap_handler_response(any()) | 
      {continue, soap:soap_req(), soap:soap_handler_state()}.
check_soap_conformance(Soap_req, State) ->  
    Cowboy_req = soap_req:server_req(Soap_req),
    case is_authorized(cowboy_req:header(<<"authorization">>, Cowboy_req)) of
        true ->
            {continue, Soap_req, State};
        false ->
            New_cowboy_req =
                cowboy_req:set_resp_header(<<"WWW-Authenticate">>, 
                                           <<"Basic realm=\"Soap\"">>,
                                           Cowboy_req),
            {error, 401, soap_req:set_server_req(Soap_req, New_cowboy_req), State}
    end.
```

(Note that there is actually a simpler way to set the http headers for the
response message, see below.)

### set_resp_attachments/2 ###

```erlang
set_resp_attachments(soap_req(), [soap_attachment()]) -> soap_req().
```

Add attachments to the response message.

### set_resp_http_headers/2 ###

```erlang
set_resp_http_headers(soap_req(), [http_header()]) -> soap_req().
```

Set the value of HTTP headers for the response.

### set_resp_status_code/2 ###

```erlang
set_resp_status_code(soap_req(), http_status_code()) -> soap_req().
```

Set the value of the HTTP status code. By default a value will be set in
accordance with the SOAP specification (200 for a successful request, 500
for a fault and codes from the 400 range for other error cases).


### soap_action/1 ###

```erlang
soap_action(soap_req()) -> string() | undefined.
```

Returns the value of the "SOAPAction" HTTP header.

### soap_version/1 ###


```erlang
soap_version(soap_req()) -> undefined | '1.1' | '1.2'.
```

Returns the SOAP version. Note that this is not available until the body of
the request has been read. This means that this function will return
`undefined` when called in the `check_htpp_conformance` callback.

