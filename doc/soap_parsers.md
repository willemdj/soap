# soap_parsers Reference #

- [Introduction](#introduction)
- [Data Types](#data-types)
- [Functions](#functions)

## Introduction ##
The `soap` application parses the content of the SOAP messages using the erlsom
SAX parser. This is a low level parser that translates the XML to a series
of 'events' (`startElement`, `endElement`, `characters` etc.), and calls a
callback function whenever such an event has been parsed. 

The `soap_parsers` module contains a number of callback functions that can
be used to process these SAX events, to turn them into a usable
representation of the XML message. This representation can for example be a
map, or a record.

When the `soap` application needs to parse a message, it calls a function in
the handler module to determine how the parsing should be done (see
[the documentation of the
callbacks](soap_server_callbacks.md#header_parser3)). These functions must
return a tuple consisting of a `sax_callback_fun()` and a start state. The
`soap_parsers` module contains a number of functions that return such a
tuple. In some cases these simply refer to sax_callback_funs that are part
of the erlsom distribution, but in some other cases the actual parsers are
also implemented by the `soap_parsers` module.

For example, the implementation of the  `header_parser` callback below
ensures that header blocks from the namespace
""http://example.com/contacts.xsd" are translated to records, using the
`model` that was generated from the WSDL, and header blocks from the
"security" namespace are translated to maps.

```erlang
header_parser("http://example.com/contacts.xsd", Soap_req, S) ->  
    {ok, soap_parsers:data_mapper(soap_interface:model(interface())), Soap_req, S};
header_parser("security", Soap_req, S) ->  
    {ok, soap_parsers:map(), Soap_req, S}.
```

In the description of the parsers below the result for the following XML
will be presented:

```xml
<?xml version="1.0"?>
<BookStore xmlns="http://www.books.org">
        <Book isbn="1-56592-235-2">
                <Title>My Life and Times</Title>
                <Author>Paul McCartney</Author>
                <Date>1998</Date>
                <Publisher>McMillin Publishing</Publisher>
        </Book>
        <Book isbn="0-440-34319-4">
                <Title>The Adventures of a Reluctant Messiah</Title>
                <Author>Richard Bach</Author>
                <Date>1977</Date>
                <Publisher>Dell Publishing Co.</Publisher>
        </Book>
</BookStore>
```

## Data Types

#### sax_callback_fun() ####

```erlang
sax_callback_fun() :: fun((erlsom:sax_event(), any()) -> any()).
```

## Functions ##


### simple_form/0 ###

```erlang
simple_form() -> {sax_callback_fun(), undefined}.
```

Translates to the "simple-form" as used by
[xmerl](http://erlang.org/doc/apps/xmerl/xmerl_ug.html), but in such a way
that the namespace is attached to the names of the XML elements:

```erlang
{"{http://www.books.org}BookStore",[],
     [{"{http://www.books.org}Book",
       [{"isbn","1-56592-235-2"}],
       [{"{http://www.books.org}Title",[],["My Life and Times"]},
        {"{http://www.books.org}Author",[],["Paul McCartney"]},
        {"{http://www.books.org}Date",[],["1998"]},
        {"{http://www.books.org}Publisher",[],
         ["McMillin Publishing"]}]},
      {"{http://www.books.org}Book",
       [{"isbn","0-440-34319-4"}],
       [{"{http://www.books.org}Title",[],
         ["The Adventures of a Reluctant Messiah"]},
        {"{http://www.books.org}Author",[],["Richard Bach"]},
        {"{http://www.books.org}Date",[],["1977"]},
        {"{http://www.books.org}Publisher",[],
         ["Dell Publishing Co."]}]}]}
```


### very_simple_form/0 ###

```erlang
very_simple_form() -> {sax_callback_fun(), any()}.
```

Similar to `simple_form/0`, but without the information about the namespace
in the result.

```erlang
{"BookStore",[],
     [{"Book",
       [{"isbn","1-56592-235-2"}],
       [{"Title",[],["My Life and Times"]},
        {"Author",[],["Paul McCartney"]},
        {"Date",[],["1998"]},
        {"Publisher",[],["McMillin Publishing"]}]},
      {"Book",
       [{"isbn","0-440-34319-4"}],
       [{"Title",[],["The Adventures of a Reluctant Messiah"]},
        {"Author",[],["Richard Bach"]},
        {"Date",[],["1977"]},
        {"Publisher",[],["Dell Publishing Co."]}]}]}
```

### skip/1 ###

```erlang
skip(Value::any()) -> {sax_callback_fun(), any()}.
```

`skip` ignores the XML, and returns the value of the "Value" parameter as
result.

So parsing the example XML with the parser that is created by `skip(skipped)`
returns `skipped`.


### map/0 ###

```erlang
map() -> {sax_callback_fun(), any()}.
```

Translates the XML to a map, in accordance with [converting between xml and json](http://www.xml.com/lpt/a/1658).

```erlang
#{"BookStore" => #{"Book" => [#{"@isbn" => <<"1-56592-235-2">>,
           "Author" => <<"Paul McCartney">>,
           "Date" => <<"1998">>,
           "Publisher" => <<"McMillin Publishing">>,
           "Title" => <<"My Life and Times">>},
         #{"@isbn" => <<"0-440-34319-4">>,
           "Author" => <<"Richard Bach">>,
           "Date" => <<"1977">>,
           "Publisher" => <<"Dell Publishing Co.">>,
           "Title" => <<"The Adventures of a Reluctant Messiah">>}]}},
```


### data_mapper/1 ###

```erlang
data_mapper(erlsom:model()) -> {sax_callback_fun(), any()}.
```

Translates the XML to records. The exact layout of the records depends on
the XSD. 

Within the `soap` application the representation of the types (the XSD) in the
WSDL is accessible via the `interface/0` function that is included in the
generated modules. The `erlsom:model()` information that must be passed to
the `data_mapper` is accessible using `soap_interface:model(interface())`,
see the example in the [introduction](#introduction) above.

With an XML Schema like this: 

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
            targetNamespace="http://www.books.org"
            xmlns="http://www.books.org"
            elementFormDefault="qualified">
    <xsd:element name="BookStore">
        <xsd:complexType>
            <xsd:sequence>
                <xsd:element name="Book" type="Book" minOccurs="1" maxOccurs="unbounded"/>
            </xsd:sequence>
        </xsd:complexType>
    </xsd:element>
    <xsd:complexType name="Book">
        <xsd:sequence>
            <xsd:element name="Title" type="xsd:string"/>
            <xsd:element name="Author" type="xsd:string"/>
            <xsd:element name="Date" type="xsd:string"/>
            <xsd:element name="Publisher" type="xsd:string"/>
        </xsd:sequence>
        <xsd:attribute name="isbn" type="xsd:string"/>
    </xsd:complexType>
</xsd:schema>
```

the result of the parser will look like this:

```erlang
#'BookStore'{
        'Book' = 
            [#'Book'{
                 isbn = "1-56592-235-2",'Title' = "My Life and Times",
                 'Author' = "Paul McCartney",'Date' = "1998",
                 'Publisher' = "McMillin Publishing"},
             #'Book'{
                 isbn = "0-440-34319-4",
                 'Title' = "The Adventures of a Reluctant Messiah",
                 'Author' = "Richard Bach",'Date' = "1977",
                 'Publisher' = "Dell Publishing Co."}]}
```
