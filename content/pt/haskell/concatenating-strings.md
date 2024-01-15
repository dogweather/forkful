---
title:                "Concatenando strings"
html_title:           "Haskell: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Porquê

Você já se deparou com a necessidade de unir ou agrupar pedaços de informação dentro da programação? É comum utilizar concatenado para juntar strings e formar uma nova string. Neste artigo, vamos explorar como realizar essa operação em Haskell de forma simples e eficiente.

## Como Fazer

Para concatenar strings em Haskell, podemos utilizar o operador "++" ou a função "concat". O operador "++" une duas strings em uma nova string, enquanto a função "concat" pode receber uma lista de strings e concatená-las em uma única string.

```Haskell
"Hello" ++ " World" -- Resultado: "Hello World"

concat ["Hello", " ", "World"] -- Resultado: "Hello World"
```

Podemos também utilizar funções de manipulação de strings, como "show" e "++" para inserir valores e variáveis dentro de uma string. Veja um exemplo:

```Haskell
nome = "Maria"
idade = 25

"Meu nome é " ++ nome ++ " e tenho " ++ show idade ++ " anos." -- Resultado: "Meu nome é Maria e tenho 25 anos."
```

A saída impressa é uma string que contém os valores de "nome" e "idade" inseridos na frase.

## Deep Dive

Em Haskell, strings são listas de caracteres, o que significa que as funções de listas, como "map" e "fold", também podem ser utilizadas para manipular strings. Além disso, a função "intercalate" pode ser utilizada para unir strings em uma lista e adicionar um separador entre elas.

```Haskell
map toUpper "hello world" -- Resultado: "HELLO WORLD"

foldr (++) " Programming" ["Haskell", " is", " Fun"] -- Resultado: "Haskell is Fun Programming"

intercalate ", " ["apple", "bananas", "oranges"] -- Resultado: "apple, bananas, oranges"
```

## Veja Também

- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell no Wikibooks](https://en.wikibooks.org/wiki/Haskell)
- [Curso de Programação Funcional com Haskell do Coursera](https://www.coursera.org/learn/programacao-funcional-haskell/)