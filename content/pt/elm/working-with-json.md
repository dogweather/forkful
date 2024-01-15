---
title:                "Trabalhando com json"
html_title:           "Elm: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando com dados em formato JSON em seu projeto, certamente encontrará alguns desafios ao lidar com a estrutura complexa desse formato. No entanto, trabalhar com JSON pode ser muito mais simples se você usar Elm, uma linguagem de programação com foco em front-end e que possui recursos específicos para trabalhar com esse formato de dados.

## Como fazer

Para lidar com JSON em Elm, você precisará importar o módulo `Json.Decode` e utilizar suas funções para decodificar e codificar dados em JSON.

Aqui está um exemplo simples de como decodificar um objeto JSON em um tipo de dados Elm:

```Elm
import Json.Decode exposing (..)

json = """{"nome": "Maria", "idade": 25}"""

type alias Pessoa =
    { nome: String
    , idade: Int
    }

pessoaDecoder =
    decode
        (map2 Pessoa
            (field "nome" string)
            (field "idade" int)
        )
        (string json)

result =
    case pessoaDecoder of
        Ok pessoa ->
            -- aqui você pode utilizar a pessoa, que será do tipo Pessoa
            
        Err erro ->
            -- caso ocorra algum erro na decodificação, é retornado um valor do tipo `Json.Decode.Error`
```

No exemplo acima, utilizamos as funções `field` para extrair os dados do objeto JSON e o `map2` para criar uma função que criará uma instância do tipo `Pessoa` a partir dos dados extraídos.

Se quiser converter um dado no formato Elm para JSON, utilize a função `encode`. Por exemplo:

```Elm
import Json.Encode exposing (..)

type alias Pessoa =
    { nome: String
    , idade: Int
    }

pessoa =
    { nome = "João"
    , idade = 30
    }

pessoaJson =
    encode 0
        (object
            [ ("nome", string pessoa.nome)
            , ("idade", int pessoa.idade)
            ]
        )
```

No exemplo acima, utilizamos a função `object` para criar um objeto JSON a partir dos dados da estrutura `Pessoa`.

## Mergulho profundo

Elm possui diversas funções para manipular e transformar dados em JSON. Além da `decode` e `encode`, existem outras funções úteis, como `list`, `dict` e `array`, para lidar com diferentes tipos de dados JSON.

Outro recurso interessante é a possibilidade de definir your próprios tipos personalizados para representar dados em JSON, utilizando a função `customDecoder`. Isso permite uma melhor estruturação dos dados e facilita seu uso no código.

Além disso, Elm possui um sistema de tipos forte que ajuda a garantir que seus dados em JSON estejam no formato correto, evitando erros e bugs ao executar seu código.

## Veja Também

- [Documentação oficial do Elm sobre JSON](https://guide.elm-lang.org/interop/json.html)
- [Tutorial sobre como trabalhar com dados JSON em Elm](https://www.dailydrip.com/blog/working-with-json-in-elm)
- [Um guia prático sobre como lidar com JSON em Elm](https://blog.noredink.com/post/174066163068/practical-elm-json-decoding)