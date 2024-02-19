---
aliases:
- /pt/elm/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:37.012669-07:00
description: "Trabalhar com JSON em Elm \xE9 sobre decodificar dados JSON em tipos\
  \ Elm e codificar valores Elm de volta para JSON. Esse processo \xE9 crucial para\
  \ aplica\xE7\xF5es\u2026"
lastmod: 2024-02-18 23:08:58.080899
model: gpt-4-0125-preview
summary: "Trabalhar com JSON em Elm \xE9 sobre decodificar dados JSON em tipos Elm\
  \ e codificar valores Elm de volta para JSON. Esse processo \xE9 crucial para aplica\xE7\
  \xF5es\u2026"
title: Trabalhando com JSON
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Trabalhar com JSON em Elm é sobre decodificar dados JSON em tipos Elm e codificar valores Elm de volta para JSON. Esse processo é crucial para aplicações web interagirem com APIs e fontes de dados externas, permitindo uma troca de dados sem problemas entre o cliente (Elm) e servidores ou outros serviços.

## Como Fazer:

Elm trata o manuseio de JSON com explicitação e segurança, utilizando principalmente os módulos `Json.Decode` e `Json.Encode`. Para começar a trabalhar com JSON, você primeiro precisa definir um decodificador para o seu tipo de dado. Vamos supor que estamos lidando com um objeto de perfil de usuário simples.

Primeiramente, defina seu tipo Elm:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Decodificando JSON para Elm

Para decodificar uma string JSON para o tipo `UserProfile`, crie um decodificador:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

Para decodificar um objeto JSON:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Saída de Exemplo:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Codificando Elm para JSON

Para codificar um valor Elm de volta para JSON, utilize o módulo `Json.Encode`.

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{-
Uso:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Saída de Exemplo:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Bibliotecas de Terceiros

Pacotes Elm como `elm-json-decode-pipeline` podem simplificar a criação de decodificadores usando um estilo de pipeline, o que é especialmente útil para decodificar objetos complexos.

Primeiro, adicione a biblioteca ao seu projeto:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Então, você pode simplificar a definição do decodificador assim:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Use este decodificador como antes com decodeString para decodificar strings JSON. -}
```

Esta abordagem simplifica o decodificador, tornando o código mais limpo e mais fácil de manter, especialmente à medida que as estruturas de dados se tornam mais complexas.
