---
title:    "Elm: Encontrando o comprimento de uma string"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que

Em muitas linguagens de programação, é necessário encontrar o comprimento de uma string para realizar determinadas tarefas. No entanto, encontrar o comprimento de uma string em Elm pode ser um pouco diferente do que em outras linguagens. Neste artigo, vamos explorar por que é importante ser capaz de encontrar o comprimento de uma string em Elm.

## Como fazer

Aqui está um exemplo simples de como encontrar o comprimento de uma string em Elm:

```Elm
nome = "João"
tamanho = String.length nome
```

O código acima define uma variável "nome" com o valor "João" e, em seguida, usa a função `String.length` para encontrar o comprimento da string nome e armazená-lo na variável "tamanho". O valor resultante para "tamanho" seria 4, já que a string "João" tem 4 caracteres.

## Profundidade

Encontrar o comprimento de uma string em Elm é bastante simples e fácil. No entanto, é importante observar que o comprimento é contado em caracteres, não em bytes. Isso significa que, se você estiver trabalhando com caracteres não ASCII ou emojis, o comprimento retornado pode não ser o esperado.

Outra coisa a ter em mente é que a função `String.length` retorna um inteiro, portanto, se você estiver trabalhando com uma string que contém apenas números, poderá precisar convertê-la para um tipo numérico antes de realizar qualquer operação matemática com ela.

## Veja também

- [Documentação oficial do Elm sobre strings](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Outras funções úteis para trabalhar com strings em Elm](https://guide.elm-lang.org/strings/)
- [Um tutorial abrangente sobre programação em Elm](https://www.elm-tutorial.org/pt-BR/)