---
date: 2024-01-26 04:21:20.097088-07:00
description: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language (Linguagem\
  \ M\xEDnima e \xD3bvia do Tom), \xE9 uma linguagem de serializa\xE7\xE3o de dados.\
  \ Programadores Elm a usam\u2026"
lastmod: '2024-03-13T22:44:46.522923-06:00'
model: gpt-4-0125-preview
summary: "TOML, abrevia\xE7\xE3o de Tom's Obvious, Minimal Language (Linguagem M\xED\
  nima e \xD3bvia do Tom), \xE9 uma linguagem de serializa\xE7\xE3o de dados. Programadores\
  \ Elm a usam\u2026"
title: Trabalhando com TOML
---

{{< edit_this_page >}}

## O Que & Por Quê?
TOML, abreviação de Tom's Obvious, Minimal Language (Linguagem Mínima e Óbvia do Tom), é uma linguagem de serialização de dados. Programadores Elm a usam para gerenciar dados de configuração porque ela é legível por humanos e mapeia de maneira ordenada para pares de chave-valor necessários em aplicações.

## Como Fazer:
Elm não possui um analisador de TOML embutido, mas você pode interagir com JavaScript ou usar um pacote da comunidade. Aqui está como você poderia analisar TOML usando um pacote hipotético `elm-toml`:

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

Para decodificar valores específicos:

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

A saída de amostra para `port` pode ser `Ok 8080` se a decodificação for bem-sucedida.

## Aprofundamento
TOML foi criado por Tom Preston-Werner, cofundador do GitHub, como uma linguagem simples para arquivos de configuração. Concorre com YAML e JSON; a sintaxe do TOML visa o melhor dos dois mundos com foco em ser fácil de ler e escrever por humanos.

Em Elm, para lidar com TOML, geralmente é necessário interagir com JavaScript, o que pode ser um pouco trabalhoso. Felizmente, a comunidade Elm é engenhosa, e vários pacotes de terceiros existem. O pacote hipotético `elm-toml` provavelmente usaria o `Port` de Elm para conversar com um analisador TOML JavaScript ou implementaria a análise diretamente em Elm.

O principal obstáculo em Elm é que tudo é tipificado estaticamente, então você precisará escrever decodificadores personalizados para lidar com diferentes estruturas de dados dentro de TOML, o que pode ser um pouco verboso, mas adiciona segurança.

## Veja Também
Para especificações e mais informações sobre o TOML, confira [TOML](https://toml.io).
Se você está procurando uma abordagem prática para interop Elm e JavaScript, comece com o guia oficial: [Portas Elm](https://guide.elm-lang.org/interop/ports.html).
Para pacotes da comunidade ou para contribuir, navegue em [Pacotes Elm](https://package.elm-lang.org/).
