---
title:    "Clojure: Concatenando strings"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Há muitos cenários onde pode ser necessário unir duas ou mais strings em um texto único. Por exemplo, ao criar um programa que exibe mensagens personalizadas, ou ao manipular dados em um banco de dados, a concatenação de strings é uma habilidade essencial para os programadores em Clojure.

## Como Fazer

Para concatenar strings em Clojure, use a função ```str```. Esta função pode receber qualquer número de argumentos de tipo string e irá uni-los em uma única string. Veja um exemplo abaixo:

```
(str "Olá" ", " "mundo!")
```

Este código resultará em ```"Olá, mundo!"``` como saída. Note que nós separamos cada string individual com vírgulas dentro da função ```str```.

## Mergulho Profundo

Além da função ```str```, você também pode utilizar a função ```format``` para concatenar strings. Esta função é útil quando você precisa inserir valores específicos em uma string. Veja o exemplo abaixo:

```
(format "Eu tenho %d anos de idade!" 25)
```

Este código resultará em ```"Eu tenho 25 anos de idade!"``` como saída. O valor ```%d``` dentro da string é substituído pelo número 25 que foi fornecido como argumento após a string.

## Veja Também

- [Documentação oficial do Clojure sobre strings](https://clojure.org/guides/strings)
- [Tutorial de Clojure para iniciantes](https://clojure.org/guides/getting_started)
- [Guia de sintaxe de Clojure](https://clojure.org/guides/learn/syntax)