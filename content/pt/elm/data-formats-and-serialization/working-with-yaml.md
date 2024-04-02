---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:14.328162-07:00
description: "Elm n\xE3o possui suporte nativo para YAML, um formato de serializa\xE7\
  \xE3o de dados frequentemente utilizado para arquivos de configura\xE7\xE3o ou compartilhamento\
  \ de\u2026"
lastmod: '2024-03-13T22:44:46.519766-06:00'
model: gpt-4-0125-preview
summary: "Elm n\xE3o possui suporte nativo para YAML, um formato de serializa\xE7\xE3\
  o de dados frequentemente utilizado para arquivos de configura\xE7\xE3o ou compartilhamento\
  \ de\u2026"
title: Trabalhando com YAML
weight: 41
---

## O Que & Por Quê?
Elm não possui suporte nativo para YAML, um formato de serialização de dados frequentemente utilizado para arquivos de configuração ou compartilhamento de dados, devido à sua forte ênfase em segurança de tipos e resultados previsíveis. No entanto, programadores frequentemente encontram YAML ao lidar com APIs ou configurações no desenvolvimento web, necessitando métodos confiáveis para analisar dados YAML para o ecossistema estritamente tipado de Elm para integração e manipulação sem problemas.

## Como Fazer:
Para lidar com YAML em Elm, normalmente é preciso converter YAML para JSON fora do Elm e então usar a funcionalidade de decodificador JSON integrada do Elm para trabalhar com os dados. Embora esta abordagem exija uma etapa adicional de conversão, ela aproveita o forte sistema de tipos do Elm para garantir a integridade dos dados. Ferramentas populares para conversão de YAML para JSON incluem conversores online ou serviços de backend. Uma vez que você tenha JSON, pode usar o módulo `Json.Decode` do Elm para trabalhar com os dados.

Primeiro, assumindo que você tenha os seguintes dados em YAML:

```yaml
person:
  name: Jane Doe
  age: 30
```

Converta-os para o formato JSON:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

Então, defina seu modelo e decodificador Elm:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

Para usar este decodificador para converter JSON para um tipo Elm:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("Olá, " ++ person.name ++ "!")
            
        Err _ ->
            Html.text "Ocorreu um erro durante a decodificação."
```

Saída (renderizada em uma aplicação Elm):
```
Olá, Jane Doe!
```

Essa abordagem garante que você possa trabalhar com dados YAML em Elm utilizando JSON como um formato intermediário, tirando vantagem do robusto sistema de tipos do Elm e das capacidades de decodificação JSON para manipular dados externos de forma segura e eficaz.
