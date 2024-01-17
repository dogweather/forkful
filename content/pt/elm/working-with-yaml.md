---
title:                "Trabalhando com yaml"
html_title:           "Elm: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Trabalhar com YAML é uma forma simples e eficiente de gerir e guardar dados na sua aplicação Elm. Programadores utilizam o YAML para armazenar e transmitir informações de maneira legível para humanos, facilitando a comunicação e colaboração em projetos de programação.

## Como fazer:
Usando a biblioteca `elm-community/yaml`, podemos facilmente importar e utilizar a funcionalidade do YAML em nossos projetos Elm. Veja um exemplo básico abaixo:

```elm
import Yaml exposing (..)

yamlData : String
yamlData =
"""
name: Elm
version: 0.19.1
type: Language
"""

yamlDecoder : Decode.Decoder (List Yaml.Value)
yamlDecoder =
    Yaml.decode Yaml.value

decodedValues : Result Decode.Error (List Yaml.Value)
decodedValues =
    Decode.decodeString yamlDecoder yamlData

main : Html msg
main =
    case decodedValues of
        Ok values ->
            -- fazer algo com os valores decodificados

        Err error ->
            -- lidar com o erro de decodificação
```

Onde `yamlData` é uma string contendo dados no formato YAML, que podem ser decodificados usando a função `decode` da biblioteca `yaml` e uma decodificadora adequada. O resultado será uma `Lista` de `Valor`es, que podem ser usados para alimentar sua aplicação Elm.

## Exploração Detalhada:
O YAML (acrônimo de "YAML Ain't Markup Language") é um formato de serialização de dados criado por Clark Evans em 2001. É frequentemente utilizado em aplicações web para descrever dados de configuração ou de estado. Alternativas ao YAML incluem JSON e XML, mas muitos programadores preferem o YAML por ser mais legível e fácil de usar.

A biblioteca `yaml` foi desenvolvida pela comunidade Elm e se baseia na biblioteca JavaScript `js-yaml`, mantida por nodeca. Ela oferece diversas funcionalidades além da função de decodificação, tais como codificação, validação e manipulação de dados YAML. Para mais informações e exemplos, consulte a documentação oficial da biblioteca `yaml`.

## Veja também:
- [Documentação da biblioteca `yaml`](https://package.elm-lang.org/packages/elm-community/yaml/latest/)
- [Formato YAML](https://yaml.org/)