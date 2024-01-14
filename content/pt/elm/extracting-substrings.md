---
title:    "Elm: Extraindo subcadeias"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que extrair substrings em Elm?

Extrair substrings é uma tarefa comum quando se trabalha com strings em qualquer linguagem de programação, incluindo Elm. Em Elm, essa tarefa pode ser realizada de maneira simples e eficiente graças às funções embutidas para manipulação de strings. Neste artigo, discutiremos por que e como extrair substrings em Elm, além de fornecer uma visão mais profunda sobre esse processo.

## Como fazer em Elm?

Para extrair uma substring em Elm, usamos a função `slice` que aceita três argumentos: a string original, o índice inicial e o índice final. O índice inicial indica a posição do primeiro caractere a ser incluído na substring e o índice final indica a posição do último caractere a ser incluído. Esses índices devem ser números inteiros e podemos fornecer um valor negativo para indicar uma posição a partir do final da string. Também podemos usar a função `substring` que possui argumentos semelhantes, mas aceita o comprimento da substring em vez do índice final.

**Exemplo 1:**

```Elm
slice "Hello World" 0 4
```

**Saída:**

`Hello`

Neste exemplo, a substring é extraída da posição 0 até a posição 4 (não inclusa), resultando em `"Hello"`.

**Exemplo 2:**

```Elm
substring "Hello World" 6 5
```

**Saída:**

`World`

Neste exemplo, a substring é extraída a partir da posição 6 com comprimento 5, resultando em `"World"`.

## Mergulho Profundo

Ao trabalhar com strings em Elm, é importante ter em mente que elas são imutáveis, ou seja, não podemos modificá-las diretamente. Portanto, ao extrair substrings, na verdade estamos criando uma nova string com base na string original.

Também vale mencionar que a indexação de caracteres em Elm começa a partir do 0. Isso significa que o primeiro caractere em uma string tem índice 0, o segundo tem índice 1 e assim por diante.

Além disso, ao fornecer um índice final maior que o comprimento da string, a função irá extrair a substring até o final da string. Da mesma forma, se o índice final for menor que o índice inicial, a função retornará uma string vazia.

## Veja também

- [Documentação oficial de strings em Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Guia rápido sobre strings em Elm](https://elmprogramming.com/quick-guide-creating-and-manipulating-strings-in-elm.html)
- [Tutorial sobre manipulação de strings em Elm](https://www.codementor.io/@seanhess/elm-tutorial-build-a-program-to-generate-symlink-structures-33zvvui38)

Esperamos que este artigo tenha sido útil para você entender por que e como extrair substrings em Elm. Com essa técnica, você pode manipular strings de maneira mais eficiente e criar lógica mais complexa em suas aplicações em Elm. Fique à vontade para explorar outras funções para manipulação de strings em Elm e aprimorar suas habilidades como programador.