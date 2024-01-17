---
title:                "Concatenando strings"
html_title:           "Elm: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que e por que? 
Concatenar strings é o processo de combinar várias strings em uma única string. Os programadores muitas vezes fazem isso para criar mensagens personalizadas, construir URLs dinamicamente ou formatar dados.

## Como fazer:
Elm torna a concatenação de strings simples através do uso do operador de adição (+) ou da função "append". Confira os exemplos abaixo:

```
Elm "Hello" + " World"
--Output: "Hello World"

Elm append ("Hello", " World")
--Output: "Hello World"
```

## Mergulho Profundo:
Nem sempre foi tão fácil concatenar strings. Em linguagens de programação mais antigas, como C, era necessário declarar o tamanho da string antecipadamente, o que podia ser um processo complicado. Além disso, existem alternativas para a concatenação de strings, como a interpolação de strings, onde as variáveis são incorporadas diretamente em uma string, em vez de serem concatenadas. A implementação da concatenação de strings em Elm é baseada em uma biblioteca JavaScript chamada "mconcat".

## Veja Também:
- Documentação do Elm para a concatenação de strings: https://guide.elm-lang.org/language/data_structures.html#strings 
- Exemplo de interpolação de strings em Elm: https://ellie-app.com/3FTT3CS5PS6a1