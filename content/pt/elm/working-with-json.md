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

## O que é e por que? 
Trabalhar com JSON é uma forma de armazenar e compartilhar dados de forma eficiente entre sistemas e aplicativos. Programadores frequentemente usam JSON para transferir informações entre clientes e servidores, sendo especialmente útil para aplicações web e mobile.

## Como fazer:
Para trabalhar com JSON em Elm, é necessário importar o módulo ```Json.Decode``` e, em seguida, usar a função ```decode``` para transformar uma string JSON em uma estrutura de dados Elm. Por exemplo:

```
import Json.Decode exposing (..)

type alias User = 
    { name : String
    , age : Int
    }

decodeUser : String -> Result String User
decodeUser json =
    decodeUser
        |> string
        |> decode

user : String
user = 
    """
    {
        "name": "João",
        "age": 25
    }
    """

decodeUser user -- Output: Ok { name = "João", age = 25 }
```

## Mergulho Profundo:
JSON é um acrônimo para JavaScript Object Notation e foi criado como um formato de dados leve e fácil de ler para armazenar e transferir informações entre sistemas e linguagens de programação. Embora seja amplamente utilizado, existem alternativas, como XML e YAML, que possuem recursos semelhantes e também são populares. Para trabalhar com JSON em Elm, é importante entender como a função ```decode``` funciona e como criar decoders personalizados para estruturar os dados da forma desejada.

## Veja também:
- [Documentação oficial do módulo Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [Tutorial sobre JSON em Elm](https://guide.elm-lang.org/effects/json.html)