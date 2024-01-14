---
title:                "Elm: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Por que usar JSON na programação Elm

JSON, ou JavaScript Object Notation, é um formato de arquivo que tem se popularizado na web como forma de trocar dados entre diferentes sistemas. Na programação Elm, trabalhar com JSON pode ser extremamente útil para acessar e manipular informações vindas de APIs de outros aplicativos ou serviços.

## Como fazer

Para trabalhar com JSON em Elm, é necessário primeiro entender a sua estrutura básica. JSON consiste em chaves e valores, onde os valores podem ser strings, números, arrays ou outros objetos. Para criar um objeto JSON em Elm, podemos utilizar a função `Json.Encode.object`, fornecendo como argumento uma lista de pares chave-valor.

```
Elm
Json.Encode.object 
    [ ( "nome", Json.Encode.string "João" )
    , ( "idade", Json.Encode.int 30 )
    ]
-- {"nome": "João", "idade": 30}
```

Da mesma forma, para decodificar um objeto JSON em Elm, podemos utilizar a função `Json.Decode.at`, passando como argumento a chave do valor que queremos acessar e o tipo de dados esperado.

```
Elm
jsonString = "{\"nome\": \"João\", \"idade\": 30}"
Json.Decode.at [ "idade" ] Json.Decode.int jsonString
-- 30
```

Outra forma de trabalhar com JSON em Elm é através do uso de bibliotecas externas, como [elm-json-decode-pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/) ou [elm-decode-pipeline](https://package.elm-lang.org/packages/eeue56/elm-decode-pipeline/latest/). Essas bibliotecas fornecem funções que auxiliam na decodificação de objetos complexos.

## Mergulho profundo

Embora a estrutura básica de JSON seja simples, pode ser complicado lidar com dados mais complexos, como arrays de objetos ou valores opcionais. Para esses casos, é importante entender como utilizar a função `map` do módulo `Json.Decode`, que permite a aplicação de transformações nos dados decodificados.

Além disso, também é importante conhecer as funções `Json.Decode.oneOf` e `Json.Decode.maybe` para lidar com casos onde os valores não são sempre os mesmos ou podem estar ausentes.

## Veja também

- [Documentação oficial do Elm sobre JSON](https://guide.elm-lang.org/effects/json.html)
- [Tutorial sobre como trabalhar com JSON em Elm](https://thoughtbot.com/blog/json-parsing-in-elm)
- [Repositório com exemplos de trabalho com JSON em Elm](https://github.com/elm-community/json-extra)