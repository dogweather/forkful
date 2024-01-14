---
title:    "Gleam: Concatenando strings"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma tarefa comum em muitas linguagens de programação e pode ser útil quando precisamos combinar pedaços de texto em um único valor. É uma habilidade importante a ser dominada para criar aplicativos eficientes e funcionais.

## Como Fazer

Em Gleam, a concatenação de strings pode ser feita de várias maneiras, mas a mais comum é usar o operador de adição (+) para combinar duas ou mais strings. Vamos ver um exemplo:

```
Gleam
    |> "Bem-vindo ao "
    +> "meu blog de programação!"
```

Este código irá concatenar as duas strings e produzir o seguinte resultado:

```
"Bem-vindo ao meu blog de programação!"
```

Também é possível utilizar a função `String.concat` para concatenar uma lista de strings. Por exemplo:

```
Gleam
    |> String.concat([ "Olá ", "mundo", "!" ])
```

Este código irá produzir o seguinte resultado:

```
"Olá mundo!"
```

Outra opção é utilizar a função `IO.concat` para concatenar e imprimir as strings diretamente em um arquivo ou na tela.

## Profundidade

A concatenação de strings pode trazer alguns desafios quando se trabalha com grandes quantidades de dados. Isso porque o processo de adicionar uma string a outra requer a cópia de todo o conteúdo da string original para a nova. Em alguns casos, isso pode levar a problemas de desempenho. Por isso, é importante considerar alternativas quando lidando com grandes volumes de dados.

Veja mais informações sobre concatenação de strings em Gleam na documentação oficial: [https://gleam.run/book/core/string.html#concatenating-strings](https://gleam.run/book/core/string.html#concatenating-strings)

## Veja Também

- [Documentação oficial para concatenar strings em Gleam](https://gleam.run/book/core/string.html#concatenating-strings)
- [Exemplos de concatenação de strings em diferentes linguagens de programação](https://www.geeksforgeeks.org/program-string-concatenation/)