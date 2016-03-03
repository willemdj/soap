## Generating a WSDL from Erlang type specifications ##

Using `soap:erlang2wsdl/3` it is possible to generate a WSDL from Erlang type
specifications. However, this can only work if there is a possible mapping
between the type specifications and the way the `soap` application works.

The following things have to be considered:

* The operations must be specified as functions with 3 arguments: 
  1. a record that contains the decoded soap body
  2. a `soap_req` that provides additional information about the request
  3. the "handler state", an argument of any type that can be used by the
  handler module to pass information between callbacks.

* The operations must have a `soap_handler_response()` type as result. This
  is a parameterised type, the parameter indicates the type of record that
  is used to pass the body of the soap result.

* The references to the record types in the function specs must either be
  references to the actual records (of the form `#record{}`, where `record`
  is the name of the record), or references to types (of the form `type()`),
  but the second case will only work if the type is a record, and they have
  the same name.
  
  Example:

  ```erlang
  -spec store(Parsed_body::#person{},
      Soap_req::soap:soap_req(), State::soap:soap_handler_state())
      -> soap:soap_handler_response(contacts_response()).

  -record(person, {
        first_name :: string(),
        last_name  :: string()}).

  -record(contacts_response, {
        result = "ok" :: string()}).

  -type contacts_response() :: #contacts_response{}.
  ```

* For the records the following built-in types can be used:
  - integer()
  - string()
  - boolean()
  - float()
  - non_neg_integer()
  - pos_integer()
  - neg_integer()

* Lists and unions can be used to structure things (no tuples).

* All fields will be optional, except if you provide a default value (this is 
conform the meaning of the type specs - implicitly record fields are always
optional unless they have a default value). This is often not what you 
want in the XSD. It is easy to fix this in the resulting XSD.

* It is possible to indicate which fields of a record have to be implemented 
as XML attributes by giving them a name that starts with '@': '@attribute'.
_Note:_ only the first (couple of) elements of the record can be 
declared as attributes.

* If you use fields that have a union (choice) data type, this will result
in "choice" element in the xsd that does not have a name (XML Schema does not
support names for choices).  Therefore a .hrl file that is generated from
the xsd will contain a field with the name "choice". This means that you
must fix the .hrl file in such cases. (Or call it "choice" to begin
with...).

* The generated WSDL will be of WSDL version 1.1 and SOAP version 1.1.

A more comprehensive example, "contacts.hrl":

```erlang
-spec store(Parsed_body::#person{},
    Soap_req::soap:soap_req(), State::soap:soap_handler_state())
    -> soap:soap_handler_response(contacts_response()).

-record(person, {
        '@id' :: integer(),          %% will be an attribute "id" in the XML
        first_name :: string(),
        last_name = "" :: string(),  %% will mandatory in the XSD
        children :: [#child{}],
        married :: boolean(),
        age :: non_neg_integer(),
        height :: float(),
        hobbies :: [string()],
        address :: #po_box{} | #home_address{}}).

-type person() :: #person{}.

-record(child, {
        name :: string(),
        age :: non_neg_integer()}).

-record(po_box, {
        number :: integer(),
        city :: string(),
        zip :: string()}).

-record(home_address, {
        street :: string(),
	number :: integer(),
        city :: string(),
        zip :: string()}).

-record(contacts_response, {
        result = "ok" :: string()}).

-type contacts_response() :: #contacts_response{}.
```

Using `soap:erlang2wsdl("contacts.hrl", "contacts", "http://localhost:8080",
[{target_namespace, "example.com/contacts"}])` this will be translated to
the following WSDL (in the file "contacts.wsdl"):

```xml
<wsdl:definitions xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" targetNamespace="example.com/contacts">
    <wsdl:types>
        <xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" targetNamespace="example.com/contacts" elementFormDefault="qualified" attributeFormDefault="unqualified">
            <xsd:element xmlns:tns="example.com/contacts" name="contacts_response" type="tns:contacts_response"/>
            <xsd:element xmlns:tns="example.com/contacts" name="home_address" type="tns:home_address"/>
            <xsd:element xmlns:tns="example.com/contacts" name="po_box" type="tns:po_box"/>
            <xsd:element xmlns:tns="example.com/contacts" name="child" type="tns:child"/>
            <xsd:element xmlns:tns="example.com/contacts" name="person" type="tns:person"/>
            <xsd:complexType name="contacts_response">
                <xsd:sequence>
                    <xsd:element name="result" type="xsd:string"/>
                </xsd:sequence>
            </xsd:complexType>
            <xsd:complexType name="home_address">
                <xsd:sequence>
                    <xsd:element name="street" type="xsd:string" minOccurs="0"/>
                    <xsd:element name="number" type="xsd:integer" minOccurs="0"/>
                    <xsd:element name="city" type="xsd:string" minOccurs="0"/>
                    <xsd:element name="zip" type="xsd:string" minOccurs="0"/>
                </xsd:sequence>
            </xsd:complexType>
            <xsd:complexType name="po_box">
                <xsd:sequence>
                    <xsd:element name="number" type="xsd:integer" minOccurs="0"/>
                    <xsd:element name="city" type="xsd:string" minOccurs="0"/>
                    <xsd:element name="zip" type="xsd:string" minOccurs="0"/>
                </xsd:sequence>
            </xsd:complexType>
            <xsd:complexType name="child">
                <xsd:sequence>
                    <xsd:element name="name" type="xsd:string" minOccurs="0"/>
                    <xsd:element name="age" type="xsd:nonNegativeInteger" minOccurs="0"/>
                </xsd:sequence>
            </xsd:complexType>
            <xsd:complexType name="person">
                <xsd:sequence>
                    <xsd:element name="first_name" type="xsd:string" minOccurs="0"/>
                    <xsd:element name="last_name" type="xsd:string"/>
                    <xsd:element xmlns:tns="example.com/contacts" name="children" type="tns:child" minOccurs="0" maxOccurs="unbounded"/>
                    <xsd:element name="married" type="xsd:boolean" minOccurs="0"/>
                    <xsd:element name="age" type="xsd:nonNegativeInteger" minOccurs="0"/>
                    <xsd:element name="height" type="xsd:float" minOccurs="0"/>
                    <xsd:element name="hobbies" type="xsd:string" minOccurs="0" maxOccurs="unbounded"/>
                    <xsd:choice minOccurs="0">
                        <xsd:element xmlns:tns="example.com/contacts" name="po_box" type="tns:po_box"/>
                        <xsd:element xmlns:tns="example.com/contacts" name="home_address" type="tns:home_address"/>
                    </xsd:choice>
                </xsd:sequence>
                <xsd:attribute name="id" type="xsd:integer"/>
            </xsd:complexType>
        </xsd:schema>
    </wsdl:types>
    <wsdl:message name="contacts_responseOut">
        <wsdl:part xmlns:tns="TargetNamespace" name="parameters" element="tns:contacts_response"/>
    </wsdl:message>
    <wsdl:message name="personIn">
        <wsdl:part xmlns:tns="TargetNamespace" name="parameters" element="tns:person"/>
    </wsdl:message>
    <wsdl:portType name="contactsPortType">
        <wsdl:operation name="store">
            <wsdl:input xmlns:tns="TargetNamespace" message="tns:personIn"/>
            <wsdl:output xmlns:tns="TargetNamespace" message="tns:contacts_responseOut"/>
        </wsdl:operation>
    </wsdl:portType>
    <wsdl:binding xmlns:tns="TargetNamespace" name="contactsBinding" type="tns:contactsPortType">
        <soap:binding xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" transport="http://schemas.xmlsoap.org/soap/http"/>
        <wsdl:operation name="store">
            <soap:operation xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" soapAction="store" style="document"/>
            <wsdl:input>
                <soap:body xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" use="literal"/>
            </wsdl:input>
            <wsdl:output>
                <soap:body xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" use="literal"/>
            </wsdl:output>
        </wsdl:operation>
    </wsdl:binding>
    <wsdl:service name="contacts">
        <wsdl:port xmlns:tns="TargetNamespace" name="contactsSoap" binding="tns:contactsBinding">
            <soap:address xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" location="http://localhost:8080"/>
        </wsdl:port>
    </wsdl:service>
</wsdl:definitions>

```

Compiling this (`soap:wsdl2erlang("contacts.wsdl")`) will give the
following specification for the "person" type:


```erlang
-record(person, {
	id :: integer() | undefined,
	first_name :: string() | undefined,
	last_name :: string(),
	children :: [child()] | undefined,
	married :: boolean() | undefined,
	age :: non_neg_integer() | undefined,
	height :: float() | 'NaN' | 'INF' | '-INF' | undefined,
	hobbies :: [string()] | undefined,
	choice :: po_box() | home_address() | undefined}).
```

Note: 
* The fields are all explicitly marked as optional except for the
"last_name" field (due to the default value in the original hrl), 
* The name for the "address" field has changed to "choice" (because
the "address" name is not present in the XSD).
* The default value for the "last_name" field is not present (again because
it is not in the XSD).
* The type for "height" includes 'NaN', 'INF' and '-INF', because the XML
  schema built-in type float may have these values.
* the '@' is missing before the "id" field name. You can add it here, but
  you can also use the spec like this (in other words: you can add the '@'
  as a temporary thing to trigger the creation of an attribute in the
  WSDL).

