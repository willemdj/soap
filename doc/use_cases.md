# Use cases #

## Implementing a SOAP Client ##
If you have a WSDL file, `soap:wsdl2erlang` can create a module that implements 
functions for each of the operations. These functions take records as their 
arguments and produce records as results: the `soap` application will map the 
XML messages to and from these records. A .hrl file will be created with 
the definitions of the records.

Let's create a client for a simple service that is provided by 
w3schools.com. It can be used to convert temperatures from Celsius to 
Fahrenheit and vice versa. The WSDL can be downloaded from 
[here](http://www.w3schools.com/xml/tempconvert.asmx?wsdl). Save it 
as `tempconvert.wsdl`.

Generate a client module using `soap:wsdl2erlang("tempconvert.wsdl")`. You 
will be prompted to specify some additional information. The dialogue might 
look like the example below.

```
1> soap:wsdl2erlang("tempconvert.wsdl").
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
Select a number: 2

Which http client must be used?
1: ibrowse
2: inets
Select a number: 1

Which port must be implemented?
1: TempConvertSoap
2: TempConvertSoap12
3: TempConvertHttpPost
Select a number: 2

Select a prefix for URI http://www.w3schools.com/xml/
1: No prefix
2: P0
3: Specify a custom prefix
Select a number: 2
==> Generated file tempconvert_server.erl
==> Generated file tempconvert_client.erl
==> Generated file tempconvert.hrl
ok
```

This will generate three files: `tempconvert_client.erl`, 
`tempconvert_server.erl` and `tempconvert.hrl`.

:warning:  _You should not use the "No prefix" option with this WSDL,
because it contains a type called `string`. Adding the prefix "P0", as in
the example above, will create a record `P0:string`. Without the "P0" the
name of the record would be "string", which is not accepted by the
compiler._

To use the service from the Erlang shell, compile the client module 
and load the record definitions: 

```
2> c(tempconvert_client).
{ok,tempconvert_client}

3> rr("tempconvert.hrl").  
['P0:CelsiusToFahrenheit','P0:CelsiusToFahrenheitResponse',
 'P0:FahrenheitToCelsius','P0:FahrenheitToCelsiusResponse',
 'P0:string',faultcode,faultdetail,faultreason,qname,
 soap_fault_1_1,soap_fault_1_2]
```

You may also need to start your HTTP client: 
`ibrowse:start()`.  Now you can convert the temperature: 

`4> tempconvert_client:'CelsiusToFahrenheit'(#'P0:CelsiusToFahrenheit'{'Celsius' = "37.8"}, [], []).` 

The result should be: 

```erlang
{ok,200,
    [{"Cache-Control","private, max-age=0,public"},
     {"Content-Type","application/soap+xml; charset=utf-8"},
     {"Date","Thu, 18 Feb 2016 12:00:09 GMT"},
     {"Server","Microsoft-IIS/7.5"},
     {"X-AspNet-Version","4.0.30319"},
     {"X-Powered-By","ASP.NET"},
     {"Content-Length","402"}],
    [],
    #'P0:CelsiusToFahrenheitResponse'{'CelsiusToFahrenheitResult' = "100.04"},
    [],
    <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><soap:Envelope xmlns:soap=\"http://www.w3.org/2003/"...>>}
```

See the [client tutorial](soap_client_tutorial.md) for more details on the options, 
for example if you need to specify another http client, specify SOAP headers or if the message contains SOAP attachments.

### Generating example requests
Since the `soap` application has quite bit of knowledge about the structure of
the messages (from the WSDL), it is possible to generate example requests with simple
default values, to allow for easy testing.

The dialogue that resulted from the command
`soap:wsdl2erlang("tempconvert.wsdl")` included an option to generate test
stubs and skeletons.  Depending on the option selected, 1 or 2 additional 
files may be created: `tempconvert_server_test.erl` and/or
`tempconvert_client_test.erl`. 

The module `tempconvert_client_test.erl` contains a function for each
operation from `tempconvert.wsdl`. These functions call the operation with
a generated default value. Generally this value will not make much sense,
because the SOAP `soap` application knows nothing of the business logic of
the service, but it will be in accordance with the type specification.
Looking at the generated code will help you to construct a valid request. 

Below the generated function for the `FahrenheitToCelsius` operation is
shown. This example is rather trivial since the arguments for this
operation are very simple, but for more complex services these examples
can be quite helpful.

```erlang
'FahrenheitToCelsius'() -> 
    tempconvert_client:'FahrenheitToCelsius'(
        #'P0:FahrenheitToCelsius'{
            % Optional:
            'Fahrenheit' = "?"},
    _Soap_headers = [],
    _Soap_options = [{url,"http://www.w3schools.com/xml/tempconvert.asmx"}]).
```

Similarly, `tempconvert_server_test.erl` contains skeleton functions that return a
generated default value. 

## Implementing a SOAP Server ##
Again taking the w3schools' `TempConvert` service ([see 
above](#implementing-a-soap-client)) as an example, let's see how the server side can 
be implemented. 

The server module `tempconvert_server.erl` was already generated, see 
above. Compile it: 
```erlang
c("tempconvert_server.erl").
``` 
and start the server: 
```erlang
soap:start_server(tempconvert_server, [{port, 8080}]).
```

Now instruct the client to use the server on local host, port 8080: 

```erlang
tempconvert_client:'CelsiusToFahrenheit'(#'P0:CelsiusToFahrenheit'{'Celsius' = "37.8"}, 
                                         [], 
                                         [{url, "http://localhost:8080"}]).
```

The result:
```
{ok, 200,[...http headers...],[],
     #'P0:CelsiusToFahrenheitResponse'{'CelsiusToFahrenheitResult' = undefined},
     [],
     <<"<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://www.w3.org/2003/05/soap-envelope\"><SOAP-ENV:Bo"...>>}
```

The result is not correct, but that is not surprising: after all you have 
not told the server how to convert Celsius to Fahrenheit. In order to do 
that you will have to add some code to `tempconvert_server.erl`.

The [SOAP server tutorial](soap_server_tutorial.md) shows you what needs to be done to make the service work properly.

```erlang
soap:stop_server(tempconvert_server).
``` 
will stop the server.

## Creating a WSDL ##

If you have an Erlang function that you want to make available for a third
party, you can create a WSDL for that service from the specification of the
function. The WSDL can than be shared with the third party, and the service
can be made available using the WSDL as described above.

As described in the [tutorial](soap_server_tutorial.md), when a service
is called the `soap` application will map it to an Erlang function with a
specific signature: it must have 3 arguments, the first must be a record,
the second a `soap_req` and the third can be of any type (it is there to
pass information between the callbacks for a particular service request).
The function must return a record.

This means that you may need to do a bit of work to create a function that
has such a signature, which calls your original function. Note that this is the
same work that you would do if you would be using a WSDL provided
by a third party, or one that you had hand-crafted.

Lets assume that you have a function that stores information about a
person. The function expects a record with the information about a person
as its input parameter:


```erlang
-spec store(#person{}) -> ok | error.

-record(person, {
    first_name :: string(),
    last_name :: string(),
    age :: integer(),
    hobbies :: [string()]}).
```

To be able to expose this function you could create a simple wrapper
function and a record definition for the response:

```erlang
-record(store_response, {result :: string()}).

store_soap(Person, Soap_req, Handler_state) -> 
    Result = store(Person),
    {ok, #store_response{result = atom_to_string(Result)}, Soap_req, Handler_state}.
```

A .hrl file `"store.hrl"` with all the record specifications and the spec for the wrapper
function might look like this:

```erlang
-spec store_soap(person(), soap:soap_req(), soap:soap_handler_state())
    -> soap:soap_handler_response(store_response()).

-record(person, {
	first_name :: string(),
        last_name :: string(),
        age :: integer(),
        hobbies :: [string()]}).

-type person() :: #person{}.

-record(store_response, {result :: string()}).

-type store_response() :: #store_response{}.
```

Out of this a WSDL with service name `store` and url
`http://example.com/store` can be generated:

```erlang
soap:erlang2wsdl("store.hrl", "store", "http://localhost:8080").
```

This WSDL can be used by third parties as a formal description of the
service.  To implement the service in Erlang (either as client or server)
the relevant modules can be created using `soap:wsdl2erlang/1`.  The resulting
server will call `store_soap/3` with the appropriate arguments when a
client invokes the store_soap operation.

Note that there are more things that may have to be considered. For more
information see [generating a WSDL](generating_a_wsdl.md).

