[%i18n foo]
[%i18n bar ~x:[%i18n a_human]]
[%i18n bar ~x:[ pcdata "Jean-Michel ("
              ; pcdata @@ string_of_int id
              ; pcdata ")" ] ]
[%i18n baz]
[%i18n baz ~c:(nb > 1)]
[%i18n bu ~x:"Jean-Michel" ~n:id ]
