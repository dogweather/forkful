---
title:    "Clojure: Encontrando o comprimento de uma string"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string?

Às vezes, quando estamos lidando com dados ou textos em nossos programas, precisamos saber quantos caracteres estão presentes em uma determinada string. Isso pode ser útil em várias situações, como verificar se uma senha atende aos requisitos mínimos de comprimento ou fazer manipulações específicas em uma string com base em seu tamanho.

## Como fazer?

Para encontrar o comprimento de uma string em Clojure, podemos usar a função "count" que retorna o número de itens em uma coleção. Como uma string é uma coleção de caracteres, podemos usar essa função para encontrar seu comprimento.

Vamos dar uma olhada em alguns exemplos:

```
(count "Olá") 
; Output: 4

(count "Eu sou uma string muito longa")
; Output: 29
```

Como você pode ver, a função "count" retorna um número inteiro que representa o comprimento da string.

## Aprofundando

Agora que já vimos como encontrar o comprimento de uma string, é importante destacar algumas coisas a serem consideradas. Primeiro, a função "count" também pode ser usada para contar o número de itens em outras coleções, como listas e vetores.

Além disso, é importante notar que a função "count" não se limita apenas a strings. Podemos usá-la para contar o comprimento de qualquer coisa que seja uma coleção, incluindo mapas e conjuntos.

## Veja também

- [Documentação oficial da função "count"](https://clojuredocs.org/clojure.core/count)
- [Tutorial de Clojure para iniciantes](https://clojure.org/guides/getting_started)
- [Exemplos práticos de Clojure](https://github.com/practicalli/clojure-examples)

Espero que este artigo tenha sido útil para entender como encontrar o comprimento de uma string em Clojure. Continue explorando a linguagem e descobrindo novas e poderosas funções. Até a próxima!