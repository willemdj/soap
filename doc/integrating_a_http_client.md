# Integrating an HTTP client #

- [Introduction](#introduction)
- [Data Types](#data-types)
- [Functions](#function-index)

## Introduction ##
To use the `soap` application to build a SOAP client, an HTTP client is
required.  This document describes the integration between the HTTP client and the `soap` application.

Modules to use SOAP with Inets/httpc and ibrowse are included in the
`soap` application (`soap_client_inets`, `soap_client_ibrowse`), but you may 
want to use another HTTP client. 

The interaction between the `soap` application and the HTTP client is simple:
there is just one function that the integration module must provide:
`http_request/5`. 

Note: in order to use the new client when generating a set of modules using
wsdl2erlang, it must be specified in the options: `{http_client,
<your_client>}`. It will not show up automatically in the dialog that
appears if no HTTP client is specified explicitly.

## Data types ##

#### http_response() ####

```erlang
http_response() :: {ok, http_status_code(), [http_header()], http_body()} | 
                   {error, any()}.

http_status_code() :: integer().
header_name() :: string().
header_value() :: string().
http_body() :: iolist().
http_header() :: {header_name(), header_value()}.
```



## Functions ##

```erlang
http_request(URL::string(), Message::iolist(), Options::any(),
                   [http_header()], Content_type::string()) -> http_response().
```

This function will be called by the `soap` application when a generated SOAP client
module is used to invoke a service.

The Message is the encoded XML, the Options are options that the user of
the generated client module may have specified with their call - these are
passed on to the HTTP client.

It is the responsibility of the HTTP client integration to ensure that the
HTTP request is executed and that the response is translated to the
`http_response()` type.
