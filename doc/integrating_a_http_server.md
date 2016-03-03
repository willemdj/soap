# Integrating an HTTP server #

- [Introduction](#introduction)
- [Data Types](#data-types)
- [Functions](#function-index)

## Introduction ##
To use the `soap` application to build a SOAP server, an HTTP server is
required.  This document describes the integration between the HTTP server and the
`soap` application.

Modules to use SOAP with Inets/httpd, Mochiweb and Cowboy are provided here
(`soap_server_inets`, `soap_server_mochiweb` and `soap_server_cowboy`), but
it may be necessary to adjust them to your specific needs - for example if
you want to route specific requests to the SOAP server, or if you want to
run the server in an embedded way.  You may also
want to use another HTTP server. 

To get an understanding of what is happening during the handling of an HTTP
request, it may be helpful to look at the [illustration](soap protocol.png) 
of the process that is provided in the 
[description of the SOAP callbacks](soap_server_callbacks.md). This picture 
also shows the interaction between the `soap` application and the HTTP server.

Three things are required:

1. It must be possible to start the server. The result must be that the
   server is set up in such a way that, for relevant requests, it has the
   information that is required to use the `soap` application:

   - The actual implementation of the soap service is done in a
     handler module (see [soap server tutorial](soap_server_tutorial.md) and
     [SOAP server callbacks](soap_server_callbacks.md)). The name of this 
     module must be made available to the `soap` application.

   - There should be a possibility to pass some options to the handler module
     (these will be passed to the `init` callback of the handler module
     with each request). 

   The integration module must implement the [`start/2`](#start2) function to take care
   of this.

2. It must be possible to stop the server. The integration module must
   implement a function [`stop/0`](#stop0).

3. for every request, a number of functions that are provided by the
   `soap` application must be called. The integration module must ensure
   that these functions are called:

   - `soap_server_handler:new_req(Handler, Server, Options, Req)`. This
     creates a new `soap_req`. The arguments are:

     - `Handler :: module()` - The name of the handler module that
       implements the  [SOAP server callbacks](soap_server_callbacks.md).

     - `Server :: atom()` - The name of the server. It is made
       available in the handler module via the function
       `soap_req:server/1`. 

     - `Options :: any()` - This should be the options that were passed to
       the `start\2` function. They are passed to the `init`-callback in
       the handler module.

     - `Req :: any()` - This can be used to make the information that is
       used by the HTTP server internally, available to the handler module.
       The data is accessible for the handler module via the functions
       [`soap_req:server_req/1`](soap_req.md#server_req1) and 
       [`soap_req:set_server_req/2`](soap_req.md#set_server_req2).

   - `soap_server_handler:handle_message(Req_body, Soap_req)`. This
     function will actually execute the operation by ensuring that the
     appropriate callbacks of the handler module are called. The arguments
     are:

     - `Req_body :: binary()` - The body of the HTTP request, as a binary.

     - `Soap_req :: soap:soap_req()` - The term that was returned by
       `soap_server_handler:new_req/4`. Ideally this should have been
       updated with information about the HTTP request, see below.

     It must return a [`server_http_response()`](#server_http_response) data type.

   In addition the integration module _should_ also:
   
   - Add information to the `soap_req` about certain properties of the HTTP
     request:
     
     - The method, using `soap_req:set_method/2`;
     - The content-type header, using `soap_req:set_content_type/2`;
     - The SOAPAction header (if present), using
       `soap_req:set_soap_action/2`.
     - The HTTP body of the HTTP request, using
       `soap_req:sett_http_body/2`.


   - Call the function `soap_server_handler:check_http_conformance(Soap_req)`.  
     This will either call the
     [`check_http_conformance/2`](soap_server_callbacks.md#check_soap_conformance2)
     callback in the handler module, or if that function is not provided it
     will perform some checks in accordance with the SOAP specification, on
     method en content-type.

Note: in order to use the new server when generating a set of modules using
wsdl2erlang, it must be specified in the options: `{http_server,
<your_server>}`. It will not show up automatically in the dialog that
appears if no HTTP server is specified explicitly.

## Data types ##

#### server_http_response ####

```erlang
server_http_response() :: {ok, http_status_code(), [http_header()], http_body(), server_req()}.

http_header() :: {binary(), iodata()}.
http_status_code() :: integer().
http_body() :: iodata().
server_req() :: any().
```

This is the response from `soap_server_handler:handle_message/2`. The
integration module must create an HTTP response from this data.

## Functions ##

#### start/2 ####

```erlang
start(Handler::module(), Options::any()) -> any().
```

Start the HTTP server and set it up in such a way that the information
about handler module and the options are made available.

Note that you will have to modify the existing integration modules
(`soap_server_cowboy_1`, `soap_server_cowboy_2`, `soap_server_mochiweb` or
`soap_server_inets`) if you want to be more specific about the routing of
requests, or if you want the server to run embedded. The `start` function
in these modules (combined with the documentation for the applications)
should generally give you a good impression how to do this.

#### stop/0 ####

```erlang
stop() -> any().
```

Stop the HTTP server.


