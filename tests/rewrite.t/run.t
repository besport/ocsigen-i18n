  $ ocamlc -dparsetree -stop-after parsing -ppx 'ocsigen-i18n-rewriter --default-module DEFAULT' input.ml
  [
    structure_item (input.ml[1,0+0]..[8,192+34])
      Pstr_eval
      expression (input.ml[1,0+0]..[8,192+34])
        Pexp_apply
        expression (input.ml[1,0+0]..[1,0+11])
          Pexp_apply
          expression (input.ml[1,0+0]..[1,0+11])
            Pexp_ident "DEFAULT.Tr.foo" (input.ml[1,0+0]..[1,0+11])
          [
            <arg>
            Nolabel
              expression (input.ml[1,0+0]..[1,0+11])
                Pexp_construct "()" (input.ml[1,0+0]..[1,0+11])
                None
            <arg>
            Nolabel
              expression (input.ml[1,0+0]..[1,0+11])
                Pexp_construct "()" (input.ml[1,0+0]..[1,0+11])
                None
          ]
        [
          <arg>
          Nolabel
            expression (input.ml[2,12+0]..[2,12+30])
              Pexp_apply
              expression (input.ml[2,12+0]..[2,12+30])
                Pexp_ident "DEFAULT.Tr.bar" (input.ml[2,12+0]..[2,12+30])
              [
                <arg>
                Nolabel
                  expression (input.ml[2,12+0]..[2,12+30])
                    Pexp_construct "()" (input.ml[2,12+0]..[2,12+30])
                    None
                <arg>
                Labelled "x"
                  expression (input.ml[2,12+14]..[2,12+29])
                    Pexp_apply
                    expression (input.ml[2,12+14]..[2,12+29])
                      Pexp_ident "DEFAULT.Tr.a_human" (input.ml[2,12+14]..[2,12+29])
                    [
                      <arg>
                      Nolabel
                        expression (input.ml[2,12+14]..[2,12+29])
                          Pexp_construct "()" (input.ml[2,12+14]..[2,12+29])
                          None
                      <arg>
                      Nolabel
                        expression (input.ml[2,12+14]..[2,12+29])
                          Pexp_construct "()" (input.ml[2,12+14]..[2,12+29])
                          None
                    ]
                <arg>
                Nolabel
                  expression (input.ml[2,12+0]..[2,12+30])
                    Pexp_construct "()" (input.ml[2,12+0]..[2,12+30])
                    None
              ]
          <arg>
          Nolabel
            expression (input.ml[3,43+0]..[5,125+30])
              Pexp_apply
              expression (input.ml[3,43+0]..[5,125+30])
                Pexp_ident "DEFAULT.Tr.bar" (input.ml[3,43+0]..[5,125+30])
              [
                <arg>
                Nolabel
                  expression (input.ml[3,43+0]..[5,125+30])
                    Pexp_construct "()" (input.ml[3,43+0]..[5,125+30])
                    None
                <arg>
                Labelled "x"
                  expression (input.ml[3,43+14]..[5,125+28])
                    Pexp_construct "::" (input.ml[3,43+16]..[5,125+28]) ghost
                    Some
                      expression (input.ml[3,43+16]..[5,125+28]) ghost
                        Pexp_tuple
                        [
                          expression (input.ml[3,43+16]..[3,43+38])
                            Pexp_apply
                            expression (input.ml[3,43+16]..[3,43+22])
                              Pexp_ident "pcdata" (input.ml[3,43+16]..[3,43+22])
                            [
                              <arg>
                              Nolabel
                                expression (input.ml[3,43+23]..[3,43+38])
                                  Pexp_constant PConst_string("Jean-Michel (",(input.ml[3,43+24]..[3,43+37]),None)
                            ]
                          expression (input.ml[4,82+16]..[5,125+28]) ghost
                            Pexp_construct "::" (input.ml[4,82+16]..[5,125+28]) ghost
                            Some
                              expression (input.ml[4,82+16]..[5,125+28]) ghost
                                Pexp_tuple
                                [
                                  expression (input.ml[4,82+16]..[4,82+42])
                                    Pexp_apply
                                    expression (input.ml[4,82+23]..[4,82+25])
                                      Pexp_ident "@@" (input.ml[4,82+23]..[4,82+25])
                                    [
                                      <arg>
                                      Nolabel
                                        expression (input.ml[4,82+16]..[4,82+22])
                                          Pexp_ident "pcdata" (input.ml[4,82+16]..[4,82+22])
                                      <arg>
                                      Nolabel
                                        expression (input.ml[4,82+26]..[4,82+42])
                                          Pexp_apply
                                          expression (input.ml[4,82+26]..[4,82+39])
                                            Pexp_ident "string_of_int" (input.ml[4,82+26]..[4,82+39])
                                          [
                                            <arg>
                                            Nolabel
                                              expression (input.ml[4,82+40]..[4,82+42])
                                                Pexp_ident "id" (input.ml[4,82+40]..[4,82+42])
                                          ]
                                    ]
                                  expression (input.ml[5,125+16]..[5,125+28]) ghost
                                    Pexp_construct "::" (input.ml[5,125+16]..[5,125+28]) ghost
                                    Some
                                      expression (input.ml[5,125+16]..[5,125+28]) ghost
                                        Pexp_tuple
                                        [
                                          expression (input.ml[5,125+16]..[5,125+26])
                                            Pexp_apply
                                            expression (input.ml[5,125+16]..[5,125+22])
                                              Pexp_ident "pcdata" (input.ml[5,125+16]..[5,125+22])
                                            [
                                              <arg>
                                              Nolabel
                                                expression (input.ml[5,125+23]..[5,125+26])
                                                  Pexp_constant PConst_string(")",(input.ml[5,125+24]..[5,125+25]),None)
                                            ]
                                          expression (input.ml[5,125+27]..[5,125+28]) ghost
                                            Pexp_construct "[]" (input.ml[5,125+27]..[5,125+28]) ghost
                                            None
                                        ]
                                ]
                        ]
                <arg>
                Nolabel
                  expression (input.ml[3,43+0]..[5,125+30])
                    Pexp_construct "()" (input.ml[3,43+0]..[5,125+30])
                    None
              ]
          <arg>
          Nolabel
            expression (input.ml[6,156+0]..[6,156+11])
              Pexp_apply
              expression (input.ml[6,156+0]..[6,156+11])
                Pexp_ident "DEFAULT.Tr.baz" (input.ml[6,156+0]..[6,156+11])
              [
                <arg>
                Nolabel
                  expression (input.ml[6,156+0]..[6,156+11])
                    Pexp_construct "()" (input.ml[6,156+0]..[6,156+11])
                    None
                <arg>
                Nolabel
                  expression (input.ml[6,156+0]..[6,156+11])
                    Pexp_construct "()" (input.ml[6,156+0]..[6,156+11])
                    None
              ]
          <arg>
          Nolabel
            expression (input.ml[7,168+0]..[7,168+23])
              Pexp_apply
              expression (input.ml[7,168+0]..[7,168+23])
                Pexp_ident "DEFAULT.Tr.baz" (input.ml[7,168+0]..[7,168+23])
              [
                <arg>
                Nolabel
                  expression (input.ml[7,168+0]..[7,168+23])
                    Pexp_construct "()" (input.ml[7,168+0]..[7,168+23])
                    None
                <arg>
                Labelled "c"
                  expression (input.ml[7,168+14]..[7,168+22])
                    Pexp_apply
                    expression (input.ml[7,168+18]..[7,168+19])
                      Pexp_ident ">" (input.ml[7,168+18]..[7,168+19])
                    [
                      <arg>
                      Nolabel
                        expression (input.ml[7,168+15]..[7,168+17])
                          Pexp_ident "nb" (input.ml[7,168+15]..[7,168+17])
                      <arg>
                      Nolabel
                        expression (input.ml[7,168+20]..[7,168+21])
                          Pexp_constant PConst_int (1,None)
                    ]
                <arg>
                Nolabel
                  expression (input.ml[7,168+0]..[7,168+23])
                    Pexp_construct "()" (input.ml[7,168+0]..[7,168+23])
                    None
              ]
          <arg>
          Nolabel
            expression (input.ml[8,192+0]..[8,192+34])
              Pexp_apply
              expression (input.ml[8,192+0]..[8,192+34])
                Pexp_ident "DEFAULT.Tr.bu" (input.ml[8,192+0]..[8,192+34])
              [
                <arg>
                Nolabel
                  expression (input.ml[8,192+0]..[8,192+34])
                    Pexp_construct "()" (input.ml[8,192+0]..[8,192+34])
                    None
                <arg>
                Labelled "x"
                  expression (input.ml[8,192+13]..[8,192+26])
                    Pexp_constant PConst_string("Jean-Michel",(input.ml[8,192+14]..[8,192+25]),None)
                <arg>
                Labelled "n"
                  expression (input.ml[8,192+30]..[8,192+32])
                    Pexp_ident "id" (input.ml[8,192+30]..[8,192+32])
                <arg>
                Nolabel
                  expression (input.ml[8,192+0]..[8,192+34])
                    Pexp_construct "()" (input.ml[8,192+0]..[8,192+34])
                    None
              ]
        ]
  ]
  
