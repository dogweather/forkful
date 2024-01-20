---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Concatenar strings significa unir duas ou mais strings para formar uma nova string. Programadores fazem isso para manipular e combinar texto de maneira eficiente e rápida.

## Como fazer:

Aqui está um exemplo de como você pode concatenar strings em Elm.

```Elm
nome = "Pedro"
sobrenome = "Silva"

nomeCompleto = nome ++ " " ++ sobrenome

main =
    Html.text nomeCompleto
```

Ao executar este código, a saída será: 

```
Pedro Silva
```

## Mergulho Profundo:

Historicamente, a concatenação de strings tem sido uma importante função em programação, desde o código de baixo nível até linguagens mais abstratas. Em Elm, a concatenação é feita através do operador `++`. 

Como alternativa à concatenação, pode-se usar estruturas de dados, como listas de strings, mas isso geralmente requer mais trabalho e complexidade.

No que diz respeito a detalhes de implementação, Elm é uma linguagem funcional que garante a imutabilidade. Isso significa que as strings originais não são afetadas quando você as concatena - o resultado é uma nova string, mantendo as originais intactas.

## Veja também:

Para mais detalhes sobre a concatenação de strings e o trabalho com strings em Elm, você pode conferir os seguintes recursos:

1. [Documentação do Elm](https://package.elm-lang.org/packages/elm/core/latest/String): A documentação oficial do módulo String do Elm.

2. [Guia Prático do Elm](https://guide.elm-lang.org/): Este guia, oferecido pelo criador do Elm, Evan Czaplicki, cobre o básico da linguagem e também implementações mais detalhadas.