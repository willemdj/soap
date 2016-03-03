### Introduction ###
This tutorial shows how to create a SOAP server from an existing 
WSDL, based on an example. The main goal is to explain the various 
callback functions that the user of the `soap` application can provide.

The code for the finished example can be found [here](example_server.erl).

### Contents ###
- [The service](#the-service)
- [Generating client and server from the WSDL](#generating-client-and-server-from-the-wsdl)
- [A look at the generated files](#a-look-at-the-generated-files)
  - [The server](#the-server)
  - [The .hrl file](#the-hrl-file)
  - [The client](#the-client)
- [A first test](#a-first-test)
- [The exception callback](#the-exception-callback) 
- [A minimal first implementation](#a-minimal-first-implementation)
- [The init callback](#the-init-callback) 
- [Dealing with the Soap header](#dealing-with-the-soap-header)
  - [The header_parser callback](#the-header_parser-callback)
  - [The header callback](#the-header-callback)
  - [Headers that are described in the WSDL](#headers_that_are_described_in_the_wsdl)
- [Other callbacks](#other-callbacks)
  - [Check HTTP conformance](#check-http-conformance)
  - [Check SOAP conformance](#check-soap-conformance)
  - [Protocol-error](#protocol-error)


### The service ###
The service that we will implement allows the user to store and retrieve 
contact information.  The WSDL for this service is 
[example.wsdl](example.wsdl). The WSDL has one service (contacts_service) 
and one port (contacts_port). There are 2 operations:

1. store. This stores information of a contact and returns an id.
2. retrieve. This retrieves the contact information for a given id.

The WSDL also contains some information about SOAP headers, this will be 
covered later.

### Generating client and server from the WSDL ###
As the first step Erlang modules for the client and server implementation
are generated from the WSDL. This can be done in an 
interactive way from the Erlang shell: `soap:wsdl2erlang("example.wsdl").`

<pre><code>
1> soap:wsdl2erlang("example.wsdl").
What must be generated?
1: client
2: server
3: both
Select a number: 3

Do you want to generate test stubs/skeletons?
1: no
2: yes, client only
3: yes, server only
4: yes, client and server
Select a number: 1

Which http server must be used?
1: cowboy_v1
2: cowboy_v2
3: inets
4: mochiweb
Select a number: 1

Which http client must be used?
1: ibrowse
2: inets
Select a number: 1

Select a prefix for URI http://example.com/contacts.xsd
1: No prefix
2: P0
3: Specify a custom prefix
Select a number: 1
==> Generated file example_server.erl
==> Generated file example_client.erl
==> Generated file example.hrl
ok
</code></pre>

It is possible to avoid the dialog by providing the options explicitly in 
the command. In this case that would have been:
```erlang
soap:wsdl2erlang("example.wsdl", [{service,"contacts_service"},
                                  {port,"contacts_port"},
                                  {generate,both},
                                  {generate_tests,none},
                                  {namespaces,[{"http://example.com/contacts.xsd",undefined}]},
                                  {http_server,soap_server_cowboy_1},
                                  {server_name,"example_server"},
                                  {http_client,soap_client_ibrowse},
                                  {client_name,"example_client"},
                                  {strict,true}]).
```
Either way, the result is that 3 files are generated: `example_client.erl`, 
`example_server.erl` and `example.hrl`.

Note that the selected options are provided in a comment line in the 
generated files, to allow you to re-generate a file easily. 

### A look at the generated files ###

#### The server ####
`example_server.erl` is the starting point for implementation of the server 
part.  It contains 2 'empty' functions for the 2 operations that the 
service must implement. Let's look at one of them:


```erlang
-spec store(Parsed_body::contact(),
    Soap_req::soap:soap_req(), State::soap:soap_handler_state())
    -> soap:soap_handler_response(id()).
store(_Parsed_body, Soap_req, State) ->
    %% your implementation goes here
    {ok, #id{}, Soap_req, State}.
```


Note that specifications for the functions are also provided. They show 
that each function has 3 arguments:

1. `Parsed_body`: This is the translation of the XML message that is 
received from the client. In the example above this is of type `contact()`.  
This type is specified in the generated .hrl file, "example.hrl".

1.  `Soap_req`: This is an opaque data type that makes it possible to 
extract and/or modify some specific information regarding the request. 
Access to this information is via functions exported by the module 
[soap_req](soap_req.md).

1. `State`: This is an argument that is passed between the callback 
functions for a request. Its value is not used by the `soap` application itself, it 
is simply passed on to the next callback.

The function must return a `soap_handler_response`. This is a parametrized type. 
The type of the (not yet encoded) message that must be sent back to the 
client is provided as the parameter value. In this case it is `id()`, which 
can again be found in "example.hrl".

Note that there are other forms of the `soap_handler_response` that will
allow you to reply with a SOAP fault or an HTTP error. They are described in
[SOAP server reference](soap_server_callbacks.md#soap_responsebody).

#### The .hrl file ####
The .hrl file that is generated ("example.hrl" in this case) contains the 
declarations of the types that are used by the server and the client.

Looking at "soap.hrl" shows the following definition of `contact()`:


```erlang
-record(contact, {
	id :: integer() | undefined,
	first_name :: string(),
	last_name :: string(),
	projects :: [string()] | undefined}).

-type contact() :: #contact{}.
```

This corresponds with the definition in the WSDL:


```xml
  <xsd:element name="contact">
    <xsd:complexType>
      <xsd:sequence>
        <xsd:element name="id" type="xsd:integer" minOccurs="0"/>
        <xsd:element name="first_name" type="xsd:string"/>
        <xsd:element name="last_name" type="xsd:string"/>
        <xsd:element name="projects" type="xsd:string" 
                     minOccurs="0" maxOccurs="unbounded"/>
      </xsd:sequence>
    </xsd:complexType>
  </xsd:element>
```

How this mapping works in detail is explained in the [erlsom 
documentation](https://github.com/willemdj/erlsom), but the example shows 
the general idea.

The .hrl file also contains a macro, `INTERFACE`. The value of this macro 
contains an internal representation of the WSDL. It is used by the `soap` application.

#### The client ####
`example_client.erl` contains functions to call each of the operations. For example for the "store" operation:


```erlang
-spec store(Soap_body::contact(),
  Soap_headers::[soap:soap_header()],
  Options::[any()]) -> soap:soap_response(id()).
store(Soap_body, Soap_headers, Options) ->
  soap_client_util:call(Soap_body, Soap_headers, Options, "\"store\"", interface()).
```

As with the server file, the specifications of the functions are also 
provided, and again they use types that are declared in the .hrl file.

Besides the `contact()` type, there are 2 more parameters for each 
operation:

1. Soap_headers. This can be used to specify zero or more SOAP header 
blocks.

2. Options. This can be used to pass options to the SOAP client.

For each of these options we will see examples below.


### A first test ###
Let's start the server and see what happens if we send a request using the 
client.

<pre><code>
2> c(example_server).
{ok,example_server}

3> rr("example.hrl").
['DebuggingHeader','LocaleOptions',contact,faultcode,
 faultdetail,faultreason,id,qname,soap_fault_1_1, soap_fault_1_2]

4> soap:start_server(example_server).
{ok,&lt;0.2234.0>}

5> c(example_client).
{ok,example_client}

6> ibrowse:start().
{ok, &lt;0.2349.0>}

7> example_client:store(#contact{first_name="Willem", last_name="de Jong", 
projects = ["soap", "xml"]}, [], []).
{fault,500,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 09:45:56 GMT"},
     {"content-length","272"}],
    [],
    #soap_fault_1_1{
        faultcode = 
            #faultcode{
                uri = "http://schemas.xmlsoap.org/soap/envelope/",
                code = "Server",subcode = undefined},
        faultstring = "exception in handler module",
        faultactor = undefined,detail = []},
    []}
</code></pre>

It works to some extent - we received a valid SOAP response, but it is a 
fault. 500 is the HTTP response code that indicates an error, and the 
`faultstring` indicates that there was a problem in the handler module - that is, 
`soap_server.erl`.

The information that is actually exchanged "on the wire" between the client 
and the server is listed below:

```xml
<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
  <s:Body>
    <erlsom:contact xmlns:erlsom="http://example.com/contacts.xsd">
      <first_name>Willem</first_name>
      <last_name>de Jong</last_name>
      <projects>soap</projects>
      <projects>xml</projects>
    </erlsom:contact>
  </s:Body>
</s:Envelope>
```

```xml
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
  <SOAP-ENV:Body>
    <SOAP-ENV:Fault xmlns="">
      <faultcode>SOAP-ENV:Server</faultcode>
      <faultstring>exception in handler module</faultstring>
    </SOAP-ENV:Fault>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>
```

Unfortunately the error message is not very helpful. What can we do to 
provide more information in the fault message?

### The exception callback ###

When an exception occurs during the handling of a request, the `soap` application 
will call the `exception` callback, if the handler module provides one.

The handler module that is generated by `wsdl2erlang` does not contain an 
exception callback function, therefore in the example above we got the 
default, rather generic result. If we add the following function to 
`example_server.erl` and compile, we will get information about the stack 
trace in the fault message.


```erlang
-export([exception/7]).

-spec exception(Class::atom(), Reason::any(), Stacktrace::any(),
                soap:soap_fault_code(), Description::string(), soap:soap_req(), 
                soap:soap_handler_state()) -> soap:soap_handler_response().
exception(Class, Reason, Stacktrace, _Type, _Desc, Soap_req, Handler_state) ->
    Message = io_lib:format("exception, class: ~p, reason: ~p,~nstack: ~P~n", 
                            [Class, Reason, Stacktrace, 14]),
    {fault, soap_req:make_fault(server, Message, Soap_req), Soap_req, Handler_state}. 
```

Compiling and calling the service again gives the result below (only part
of the stack trace is shown, the actual response is longer):


<pre><code>
8> c(example_server).
{ok,example_server}

9> example_client:store(#contact{first_name="Willem", last_name="de Jong", 
projects = ["soap", "xml"]}, [], []).
{fault,500,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 10:35:42 GMT"},
     {"content-length","1133"}],
    [],
    #soap_fault_1_1{
        faultcode = 
            #faultcode{
                uri = "http://schemas.xmlsoap.org/soap/envelope/",
                code = "Server",subcode = undefined},
        faultstring = 
            "exception, class: throw, reason: {error,\n
                \"No value provided for non-optional element [id]\\n\"},\
                ...
        faultactor = undefined,detail = []},
    []}
</code></pre>

Aha, that explains it: the response that is given by the `store` callback
in the generated module looks like this: 

```erlang
    {ok, #id{}, Soap_req, State}.
```

But the declaration of the `#id{}` looks like this:


```erlang
-record(id, {
	id :: integer()}).
```

So the `id` field must get an integer value. This can also be seen by
looking at the WSDL, see below - the `id` element is mandatory (otherwise
there should have been a `minOccurs="0"` attribute).


```xml
      <xsd:element name="id">
         <xsd:complexType>
           <xsd:sequence>
             <xsd:element name="id" type="xsd:integer"/>
           </xsd:sequence>
         </xsd:complexType>
      </xsd:element>
```

It looks like we really have to provide a proper implementation of the
`store` operation.

## A minimal first implementation ##
To get going quickly, create an ETS table to store the contacts.

Replace the implementation of the `store` and `retrieve` operations like
this:


```erlang
store(Parsed_body, Soap_req, State) ->
    Id = ets:info(contacts, size) + 1,
    true = ets:insert(contacts, Parsed_body#contact{id = Id}),
    {ok, #id{id=Id}, Soap_req, State}.

retrieve(#id{id=Id}, Soap_req, State) ->
    [Contact] = ets:lookup(contacts, Id),
    {ok, Contact, Soap_req, State}.
```

Create the table and try the `store` operation again:


```
10> ets:new(contacts, [set, named_table, {keypos, #contact.id}, public]).
contacts

11> example_client:store(#contact{first_name="Willem", last_name="de Jong", projects = ["soap", "xml"]}, [], []). 
{ok,200,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 10:44:54 GMT"},
     {"content-length","209"}],
    [],
    #id{id = 1},
    []}
```

And we can retrieve it again:


```
12> example_client:retrieve(#id{id=1}, [], []). 
{ok,200,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 10:45:25 GMT"},
     {"content-length","329"}],
    [],
    #contact{id = 1,first_name = "Willem",last_name = "de Jong",
             projects = ["soap","xml"]},
    []}
```

But it is silly that the ets table has to be created from the shell,
perhaps we can let the server check if it exists, and create it if
necessary. 

## The init callback ##
The `soap` application will try to call a callback function called `init` as the
very first thing when it receives a SOAP request. Here we can check if the
ets table exists, and create it if necessary.



```erlang
-export([init/2]).

%% Ensures that the ETS table exists, and links it to the process that 
%% started the server (because otherwise it will disappear between requests).
-spec init(soap:soap_req(), Options::any()) -> 
    {soap:soap_req(), soap:soap_handler_state()}.
init(Soap_req, [Pid]) -> 
    case ets:info(contacts) of
        undefined ->
          ets:new(contacts, [set, named_table, {keypos, #contact.id}, 
                  {heir, Pid, []}, public]);
        _ ->
            ok
    end,
    {Soap_req, []}.
```

This illustrates another point: we can pass some information to the `init`
callback. Here we are passing a pid, because we need to link the table to a
process that continues to exist (the `heir` option in the `ets:new` call).
Without that the table would disappear after every call.

`soap:start_server` has an optional second argument that is passed to the
`init` callback as the value of the Options argument. Below we use the pid
of the shell (`self()`), so that the ets table is linked to the shell
process.


```
13> soap:stop_server(example_server).
ok

14> ets:delete(contacts).
true

15> c(example_server).  %% recompile
{ok,example_server)

16> soap:start_server(example_server, [self()]). %% link the table to shell
{ok,<0.3679.0>}

17> example_client:store(#contact{first_name="Willem", last_name="de Jong", projects = ["soap", "xml"]}, [], []).
{ok,200,[],#id{id = 1}}
```

### Dealing with the Soap header ###
Soap messages consist of an envelop with a body and an optional header. So
far the implementation of the contacts service has only dealt with the
body, but it might be useful to add a header that deals with authentication.

We could use a Soap header block like the one below for a simple
username/password type of authentication.


```xml
<s:credentials xmlns:s="security">
  <s:username>Willem</s:username>
  <s:password>secret</s:password>
</s:credentials>
```

Adding the header block to the request is simple:

```
18> Credentials = "<sec:credentials xmlns:sec=\"security\"><sec:username>Willem</sec:username><sec:password>secret</sec:password></sec:credentials>".
19> example_client:retrieve(#id{id=1}, [Credentials], []). 
{ok,200,[],
   ...
```

The service still gives the same response, but that is only because header
blocks are ignored by default, not because the server checked the
credentials. 

On the wire the message with the SOAP header block looks like this:


```xml
<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
  <s:Header>
    <sec:credentials xmlns:sec="security">
      <sec:username>Willem</sec:username>
      <sec:password>secret</sec:password>
    </sec:credentials>
  </s:Header>
  <s:Body>
    <id xmlns="http://example.com/contacts.xsd">
      <id>1</id>
    </id>
  </s:Body>
</s:Envelope>
```

So the header block is there, but how to parse and process it?

#### The header_parser callback ####
The header_parser callback makes it possible to specify how the header
blocks should be parsed. The reply value must be a function - more
specifically: an erlsom sax callback function, and its starting state.

According to the SOAP specification there can be more than 1 header block. Each
of them must be namespace qualified. The namespace determines how the
header block will be processed.

If you want you can create your own very specific parser function. The
details can be found in the [erlsom
documentation](#https://github.com/willemdj/erlsom#sax-mode). But in many
cases it will be possible to select one of the functions in the
`soap_parsers` module. These functions return the required combination of
sax callback function and starting state.

The example below shows how the header block can be translated to a map.

```erlang
-export([header_parser/3]).

-spec header_parser(Namespace::string(), soap:soap_req(), 
    soap:soap_handler_state()) 
    -> {ok, {fun((Event::erlsom:sax_event(), State::any()) -> any()),
             any()}, soap:soap_req(), soap:soap_handler_state()}.
header_parser("security", Soap_req, S) ->  
    {ok, soap_parsers:map(), Soap_req, S}.
```

The result will be:


```erlang
#{"credentials" => #{"password" => <<"secret">>,"username" => <<"Willem">>}}
```

That value will be passed to the header callback function. 

#### The header callback ####
The parsed header is passed to the `header` callback for processing. In
general the result of that processing should be passed to the next callback
(and eventually the functions that implement the operations) using the
handler state. 

In the example below a proplist will be used for the handler state. If the
user provides the right credentials a tuple {authorised, true}
is added to the proplist. (Note that the `init` callback provided an empty
list as initial handler state.)


```erlang
-export([header/3]).

-spec header(Parsed_header::any(), soap:soap_req(), soap:soap_handler_state()) 
  -> {ok, soap:soap_req(), soap:soap_handler_state()}.
header(#{"credentials" := Credentials}, Soap_req, State) ->
    Authorised = case Credentials of
                 #{"username" := <<"Willem">>, "password" := <<"secret">>} ->
                     true;
                 _ ->
                     false
             end,
    {ok, Soap_req, [{authorised, Authorised} | State]}.
```
  
Note that the default `header` callback ignores the header, and that this
will also happen if a header callback is provided that does not match. 
For a header block `<sec:password xmlns:sec="security">secret<\sec:password>` 
parsing would result in `#{"password" => <<"secret">>}`, which would be
ignored because there is no match.

To check whether a request is authorised we need to change the operations.
The example below shows only the new `retrieve` operation - something
similar should be done for the `store` operation.


```erlang
-spec retrieve(Parsed_body::id(),
    Soap_req::soap:soap_req(), State::soap:soap_handler_state())
    -> soap:soap_handler_response(contact()).
retrieve(#id{id=Id}, Soap_req, State) ->
    case proplists:get_value(authorised, State, false) of
        true -> 
            [Contact] = ets:lookup(contacts, Id),
            {ok, Contact, Soap_req, State};
        false ->
            Fault = soap_fault:fault(client, "Not authorised", Soap_req),
            {fault, Fault, Soap_req, State}
    end.
```

Now the result of the retrieve operation will be a fault, except if the
right credentials are provided:


```
20> c(example_server).  %% recompile
{ok,example_server)

21> example_client:retrieve(#id{id=1}, [], []).
{fault,500,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 10:57:40 GMT"},
     {"content-length","258"}],
    [],
    #soap_fault_1_1{
        faultcode = 
            #faultcode{
                uri = "http://schemas.xmlsoap.org/soap/envelope/",
                code = "Client",subcode = undefined},
        faultstring = "Not authorised",faultactor = undefined,
        detail = []},
    []}

22> example_client:retrieve(#id{id=1}, [Credentials], []). 
{ok,200,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 10:58:22 GMT"},
     {"content-length","329"}],
    [],
    #contact{id = 1,first_name = "Willem",last_name = "de Jong",
             projects = ["soap","xml"]},
    []}
```


#### Headers that are described in the WSDL ####
The "security" header block  that we looked at above is not described by the WSDL.
But the WSDL does contain a specification for the DebuggingHeader. This
means that we can use the erlsom data-mapper to parse this header block.

The type for the DebuggingHeader is in the generated "example.hrl" file,
and the client can process it:

```
23> example_client:retrieve(#id{id=1}, [#'DebuggingHeader'{debugLevel = 1}, Credentials], []).
{ok,200,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 10:59:38 GMT"},
     {"content-length","329"}],
    [],
    #contact{id = 1,first_name = "Willem",last_name = "de Jong",
             projects = ["soap","xml"]},
    []}
```

The message that is sent to the server looks like this:


```xml
<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
  <s:Header>
    <erlsom:DebuggingHeader xmlns:erlsom="http://example.com/contacts.xsd">
      <debugLevel>1</debugLevel>
    </erlsom:DebuggingHeader>
    <sec:credentials xmlns:sec="security">
      <sec:username>Willem</sec:username>
      <sec:password>secret</sec:password>
    </sec:credentials>
  </s:Header>
  <s:Body>
    <erlsom:id xmlns:erlsom="http://example.com/contacts.xsd">
      <id>1</id>
    </erlsom:id>
  </s:Body>
</s:Envelope>
```

Since there is no `header_parser` specified for the namespace
"http://example.com/contacts.xsd", this header is ignored by the server.
The example code below changes that.


```erlang
-spec header_parser(Namespace::string(), soap:soap_req(), 
    soap:soap_handler_state()) 
    -> {ok, {fun((Event::erlsom:sax_event(), State::any()) -> any()),
             any()}, soap:soap_req(), soap:soap_handler_state()}.
header_parser("http://example.com/contacts.xsd", Soap_req, S) ->  
    {ok, soap_parsers:data_mapper(soap_interface:model(interface())), Soap_req, S};
header_parser("security", Soap_req, S) ->  
    {ok, soap_parsers:map(), Soap_req, S}.
```

The `header` callback can be extended to make the new
information available:


```erlang
-spec header(Parsed_header::any(), soap:soap_req(), soap:soap_handler_state()) 
  -> {ok, soap:soap_req(), soap:soap_handler_state()}.
header(#'DebuggingHeader'{debugLevel = Level}, Soap_req, State) ->
    {ok, Soap_req, [{debug_level, Level} | State]};
header(#{"credentials" := Credentials}, Soap_req, State) ->
    Authorised = case Credentials of
                 #{"username" := <<"Willem">>, "password" := <<"secret">>} ->
                     true;
                 _ ->
                     false
             end,
    {ok, Soap_req, [{authorised, Authorised} | State]}.
```


And the operations can use it:

```erlang
-spec retrieve(Parsed_body::id(),
    Soap_req::soap:soap_req(), State::soap:soap_handler_state())
    -> soap:soap_handler_response(contact()).
retrieve(#id{id=Id}, Soap_req, State) ->
    case proplists:get_value(debug_level, State, 0) of
        1 ->
            io:format("retrieving contact ~p~n", [Id]);
        _ ->
          ok
    end,
    case proplists:get_value(authorised, State, false) of
        true -> 
            [Contact] = ets:lookup(contacts, Id),
            {ok, Contact, Soap_req, State};
        false ->
            Fault = soap_req:make_fault(client, "Not authorised", Soap_req),
            {fault, Fault, Soap_req, State}
    end.
```

So now we can trigger a simple kind of logging using the DebuggingHeader:


```
24> c(example_server).  %% recompile
{ok,example_server}

25> example_client:retrieve(#id{id=1}, [#'DebuggingHeader{debugLevel = 1}, Credentials], []). 
retrieving contact 1
{ok,200,
    [{"server","Cowboy"},
     {"date","Thu, 11 Feb 2016 11:16:40 GMT"},
     {"content-length","329"}],
    [],
    #contact{id = 1,first_name = "Willem",last_name = "de Jong",
             projects = ["soap","xml"]},
    []}
```


### Other callbacks ###
There are a couple of additional callbacks that the `soap` application may try to
call. They all deal with cases where you may want to relax (or possibly
tighten) some of the requirements that the SOAP specification sets, in
particular regarding the HTTP method and the HTTP "content-type" header.

It may be useful to have a look at the [picture](soap protocol.png) that describes how the
`soap` application calls the callback functions.

A complete example implementation of the `contacts` service that also
includes examples of two of these additional callbacks is
[here](example_server.erl). To test them the generated client module can
typically not be used, because that follows the SOAP standard, so a
standard HTTP client must be used. See below for an example.


```
21> Message =  "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">
                  <SOAP-ENV:Body>
                    <id xmlns=\"http://example.com/contacts.xsd\">
                      <id>1</id>
                    </id>
                  </SOAP-ENV:Body>
                </SOAP-ENV:Envelope>".
"<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Body><id
xmlns=\"http://example.com/contacts.xsd\"><id>1</id></id></SOAP-ENV:Body></SOAP-ENV:Envelope>"

22> ibrowse:send_req("http://localhost:8080", [{"Content-Type", "text/xml;charset=UTF-8"}], get, Message, []).
```

#### Check HTTP conformance ####
This callback is called before the body of the request is read. This means
that the `soap` application does not yet have information about the SOAP version.
Since the rules for HTTP methods and Content-Type differ between the SOAP
versions, the `soap` application cannot perform any meaningful check at this moment,
but for a specific implementation it may be useful to reject some calls in
a very early phase.

#### Check SOAP conformance ####
This callback is called as soon as the SOAP version is known. The default
implementation checks the HTTP method and Content-Type for conformance to
the standard. The handler module may replace the default implementation,
for example to deal with a client that does not follow the standard.
Note that it will often be easier to use the `protocol_error` callback, see
below.

#### Protocol error ####
This callback is called if the default conformance check results in an
error. The type of error will be passed to the callback, which may either
reply with a `soap_handler_response` or it can indicate that the request should
continue, ignoring the error.
