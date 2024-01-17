---
title:                "Extraindo substrings"
html_title:           "Haskell: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Extrair substrings significa pegar uma parte de uma string maior, geralmente com base em um padrão específico. Programadores fazem isso para manipular dados de forma mais eficiente, facilitando tarefas como busca e substituição em textos.

## Como fazer:

Extrair uma substring em Haskell é simples e pode ser feito de várias formas. Veja um exemplo usando a função ```take```, que pega os primeiros n elementos de uma lista:

```Haskell
take 3 "Exemplo de substring" 
```
A saída seria "Exe". No entanto, isso só funciona para extrair uma quantidade fixa de caracteres. Para extrair com base em um padrão, podemos usar a função ```dropWhile```, que remove elementos enquanto o predicado fornecido for verdadeiro:

```Haskell
dropWhile (/='a') "Exemplo de substring"
```
A saída seria "a substring".

## Mergulho profundo:

Extrair substrings é uma técnica antiga e amplamente utilizada em programação de computadores, com muitas funções e métodos dedicados a essa tarefa em diferentes linguagens. Em Haskell, existem outras funções além de ```take``` e ```dropWhile``` para extrair substrings, como ```substring``` e ```slice```. Além disso, é importante lembrar que strings em Haskell são tratadas como listas de caracteres, permitindo o uso de funções genéricas como ```filter``` e ```map``` para extrair substrings com base em uma condição.

## Veja também:

- [Funções de manipulação de strings em Haskell](https://www.haskell.org/hoogle/?hoogle=string)
- [Documentação oficial da linguagem Haskell](https://www.haskell.org/documentation/)
- [Tutorial prático sobre manipulação de strings em Haskell](https://wiki.haskell.org/Strings)