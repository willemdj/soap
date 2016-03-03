-spec store_soap(person(), soap:soap_req(), soap:soap_handler_state())
    -> soap:soap_handler_response(store_response()).

-record(person, {
        first_name :: string(),
        last_name :: string(),
        age :: non_neg_integer(),
        hobbies :: [string()]}).

-type person() :: #person{}.

-record(store_response, {result :: string()}).

-type store_response() :: #store_response{}.
