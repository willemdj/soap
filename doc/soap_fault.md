# soap_fault Reference #

- [Introduction](#introduction)
- [Data Types](#data-types)
- [Callback functions](#function-index)

## Introduction ##
This module contains functions to create SOAP faults. The handler module
can use these functions to create a a representation of a fault that can be
used in the soap_response. This will result in a SOAP fault being sent to
the client. (See the [SOAP server tutorial](soap_server_tutorial.md) for
more information about the use of the handler module.)

The module also contains functions that translate SOAP faults that are
received by the client to records. These functions are used internally by
the `soap` application, and therefore not described here. See the [SOAP client
tutorial](client_tutorial.md) for a description of how these records are
used when implementing a client.

It should be noted that SOAP faults can be fairly complicated data structures, and
that they are different between SOAP version 1.1 and 1.2.

An example of a fault for version 1.1:

```xml
<ENV:Envelope xmlns:ENV="http://schemas.xmlsoap.org/soap/envelope/">
  <ENV:Body>
    <ENV:Fault>
      <faultcode>ENV:Client</faultcode>                <!-- Mandatory, normally either "Client" or "Server"  -->
      <faultstring>Something went wrong</faultstring>  <!-- Mandatory, human readable explanation -->
      <faultactor>http://some/uri</faultactor>         <!-- Optional - who caused the fault -->
      <detail>                                         <!-- Optional (i.e.: may have 0 children) -->
        <P:something xmlns:P="some_namespace">Some detail<P:something>
        <P:something_else xmlns:P="some_namespace">Other detail<P:something_else>
      </detail>
    </ENV:Fault>
  </ENV:Body>
</ENV:Envelope>
```

A version 1.2 fault:

```xml
<env:Envelope xmlns:env="http://www.w3.org/2003/05/soap-envelope">
  <env:Body>
    <env:Fault>
      <env:Code>                                       <!-- ~ faultcode from the 1.1 version, -->
        <env:Value>env:Sender</env:Value>              <!-- but using different values. -->
        <env:Subcode>                                  <!-- Optional subscode -->
          <env:Value xmlns:m="some_uri">m:MessageTimeout</env:Value>
        </env:Subcode>
      </env:Code>
      <env:Reason>                                     <!-- ~ faultstring, but more than 1 can be present. -->
        <env:Text xml:lang="en-US">Something went wrong</env:Text>
      </env:Reason>
      <env:Role>http://some/uri</env:Role>             <!-- ~ faultactor -->
      <env:Detail>                                     <!-- ~ detail -->
        <P:something xmlns:P="some_uri">Some detail<P:something>
        <Q:something_else xmlns:Q="other_namespace">Other detail<Q:something_else>
      </env:Detail>
    </env:Fault>
  </env:Body>
</env:Envelope>
```

In practice it is probably often enough to provide a fault code (client or
server) and human readable description (which is transferred in the `faultstring` or
`Reason` element depending on the version). 


## Data Types

#### fault_code ####

```erlang
fault_code() :: fault_code_atom() | fault_code_object().
```

#### fault_code_atom ####

```erlang
fault_code_atom() :: server |                %% "Server" (SOAP 1.1) or "Sender" (1.2)
                     client |                %% "Client" (SOAP 1.1) or "Receiver" (1.2)
                     version_mismatch |      %% "VersionMismatch"
                     must_understand |       %% "MustUnderstand"
                     data_encoding_unknown | %% "DataEncodingUnknown"
```

Codes to be used in SOAP faults. They correspond to the SOAP Fault Codes from the spec.
`soap_fault:fault` will translate them to the appropriate string in the fault message.

If it is necessary to use a fault code that is not in the predefined set, a
fault_code_object() can be used. This can be created using
[`fault_code_1_1/2`](#fault_code_1_12) or
[`fault_code_1_2/2`](#fault_code_1_22), depending on the SOAP version.

#### fault_code_object ####

```erlang
fault_code_object()
```

An opaque data-type that is created by `code/1` or `code/2`. This can be used in
case a non-standard fault code must be used.

#### fault_string ####

```erlang
fault_string() :: string() | fault_reason() | [#fault_reason()].
```

A SOAP fault always needs a human readable bit of text that explains the
problem that occurred. The `fault_string` data type captures that
information.

In its simplest form it is simply a string. For a SOAP 1.1 request this
string will be used for the `faultstring` element. For a SOAP 1.2 request
it will be used as a `Reason` element, with the default language code "en".

In order to specify another language, a `fault_reason` can be created using
`reason/2`.

#### fault_reason ####

```erlang
fault_reason()
```

An opaque data-type that is created by `reason/2`. This can be used
to create a Reason (which is the SOAP 1.2 replacement for "faultstring")
with an "xml:lang" attribute.

#### fault_detail ####

```erlang
fault_detail()
```

An opaque data-type that is created by `detail/4`. This is used to
create values for the detail/Detail elements.

## Function Index
- [fault/3](#fault3)
- [fault/4](#fault4)
- [fault/5](#fault5)
- [code/1](#code1)
- [code/2](#code2)
- [code_with_subcode/2](#code_with_subcode2)
- [code_with_subcode/3](#code_with_subcode3)
- [detail/4](#detail4)


### fault/3 ###

```erlang
fault(fault_code(), fault_string(), soap_req()) -> iolist().
```

Creates an iolist that corresponds to the XML of the body of a SOAP fault.
The exact format of the result depends on the SOAP version (which is read
from the `soap_req` argument).

In the case of a SOAP 1.1 request, the faultcode and the faultstring
sub-elements are populated. For a SOAP 1.2 request the Code/Value and the
Reason/Text elements are set (using "en" as the default for the "xml:lang"
attribute). 

Note that this function offers a simple way to create a fault, for example:

```erlang
Fault = soap_fault(server, "Something went wrong", Soap_req)
```

### fault/4 ###

```erlang
fault(fault_code(), fault_string(), [fault_detail()], soap_req()) -> iolist().
```

As `fault/3`, but now also the detail/Detail section can be set.

Note that the `fault_detail()` arguments must be created using the
`detail/4` function.

### fault/5 ###

```erlang
fault(fault_code(), fault_string(), [fault_detail()], Actor::string(), soap_req()) -> iolist().
```

As `fault/4', but now also the faultactor/Role element can be set.


### code/1 ###

```erlang
code(Code::fault_code_atom()) -> fault_code().
```

Creates a fault code from one of the atoms `server`, `client` etc.

### code/2 ###

```erlang
code(Uri::string(), Code::string()) -> fault_code().
```

Creates a fault code.

### code_with_subcode/2 ###

```erlang
code(Code::fault_code_atom(), Subcode::fault_code()) -> fault_code().
```

Creates a fault code with a subcode (only to be used in case of SOAP
version 1.2).


### code_with_subcode/3 ###

```erlang
code(Uri::string(), Code::string(), Subcode::fault_code()) -> fault_code().
```
Creates a fault code with a subcode (only to be used in case of SOAP
version 1.2).
