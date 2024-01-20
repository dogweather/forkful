---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Encontrar o comprimento de uma string é descobrir o número de caracteres que ela contém. Programadores o fazem para manipulação de dados, validação de entradas e formatação de saídas.

## Como Fazer:

Para encontrar o comprimento de uma string no Clojure, usamos a função `count`. 

```Clojure
(count "Olá, mundo!")
```

A saída será:

```Clojure
12
```

A string "Olá, mundo!" contém 12 caracteres, incluindo os espaços e a pontuação.

## Mergulho Profundo

A função `count` no Clojure é talvez a maneira mais simples e direta de encontrar o comprimento de uma string. No entanto, ele vem da herança do Lisp, onde a função 'count' era originalmente usada para contar o comprimento de uma lista. 

Como alternativa, você poderia converter a string em uma sequência de caracteres e depois usar `reduce` para contar. Mas isso é mais complicado e menos eficiente.

Clojure implementa a função `count` em termos de `java.lang.CharSequence.length`, o que significa que funciona em tempo constante.

## Veja Também

1. Documentação do Clojure para [count](https://clojuredocs.org/clojure.core/count)
2. Diferença entre String Length e Character Count in [StackOverflow](https://stackoverflow.com/questions/2932892/difference-between-string-length-and-character-count)