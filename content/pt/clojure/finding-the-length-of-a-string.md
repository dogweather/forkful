---
title:                "Clojure: Encontrando o comprimento de uma string"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum na programação. Saber o tamanho de uma string pode ser útil para diversas situações, como a formatação de saída de dados ou para validar a entrada do usuário. Neste artigo, iremos explorar como podemos encontrar o comprimento de uma string utilizando a linguagem de programação Clojure.

## Como Fazer

Para encontrar o comprimento de uma string em Clojure, podemos utilizar a função `count` passando a string como argumento. Veja o exemplo abaixo:

```Clojure
(count "Olá, mundo!") ; retorna 12
```

O código acima vai retornar o comprimento da string "Olá, mundo!" que é igual a 12. Podemos também armazenar o valor retornado em uma variável para uso posterior, como mostrado no exemplo abaixo:

```Clojure
(def texto "Este é um texto")
(count texto) ; retorna 15
```

Além disso, é importante lembrar que a função `count` também pode ser utilizada em coleções, como listas ou vetores, não apenas em strings.

## Mergulho Profundo

Ao utilizar a função `count`, é importante entender como ela funciona internamente. Em Clojure, strings são tratadas como sequências de caracteres, o que nos permite utilizar funções de sequências, como `count`. Além disso, a função `count` é implementada nativamente em Clojure, o que significa que é uma função otimizada e rápida.

Também é relevante destacar que a função `count` conta o número de caracteres em uma string, e não o número de bytes. Isso é importante principalmente em linguagens como Clojure, onde caracteres Unicode são suportados.

## Veja Também

- [Documentação sobre a função `count`](https://clojuredocs.org/clojure.core/count)
- [Mais informações sobre strings em Clojure](https://clojure.org/api/cheatsheet#Strings)