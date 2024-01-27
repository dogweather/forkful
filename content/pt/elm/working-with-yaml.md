---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que É & Por Quê?
Trabalhar com YAML significa lidar com um formato de serialização de dados legível por humanos, comum em configurações de projetos e dados de serialização. Programadores usam YAML pela sua simplicidade e facilidade de leitura em comparação com outros formatos como XML ou JSON.

## How to:
Elm não tem suporte incorporado para YAML, mas você pode converter YAML para JSON e usar o pacote `elm/json` para manipular os dados. Veja um exemplo de conversão e uso:

```Elm
-- Elm não possui um pacote dedicado para YAML,
-- então converta YAML para JSON antes de usar com Elm.

import Json.Decode exposing (decodeString)
import Json.Decode.Pipeline exposing (required)

type alias Project =
    { name : String
    , version : String
    }

projectDecoder : Json.Decode.Decoder Project
projectDecoder =
    Json.Decode.Pipeline.decode Project
        |> required "name" Json.Decode.string
        |> required "version" Json.Decode.string

-- Suponha que "yamlToJson" é uma função (em JavaScript) que converte YAML para JSON.
-- Você pode integrar a conversão via ports.

json : String
json = "{ \"name\": \"MeuProjeto\", \"version\": \"1.0.0\" }"

result : Result String Project
result = decodeString projectDecoder json

-- O `result` seria `Ok { name = "MeuProjeto", version = "1.0.0" }` ou `Err` com a mensagem de erro de decodificação.
```

## Deep Dive
YAML, que significa "YAML Ain't Markup Language" (recursivamente), foi introduzido em 2001 como alternativa ao XML para a maioria das tarefas de configuração. Alternativas ao YAML incluem JSON e TOML, mas YAML continua popular pelos comentários fáceis e por evitar colchetes e chaves. 

A implementação em Elm depende de conversão pois não existe uma biblioteca direta para YAML. A comunidade Elm prioriza segurança e simplicidade, e trabalhar com JSON oferece ambas. Ao passar YAML como JSON para Elm, podemos tirar proveito dos decodificadores robustos de Elm para manejar os dados de forma segura.

## See Also
- [elm/json](https://package.elm-lang.org/packages/elm/json/latest/) para decodificação JSON em Elm.
- [YAML to JSON Online Converter](https://www.json2yaml.com/) para converter YAML em JSON online.
- [Curso Elm Interativo](https://guide.elm-lang.org/) para aprender mais sobre Elm.
