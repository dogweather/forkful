---
title:                "Gerando números aleatórios"
html_title:           "Clojure: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que e por que?

Gerar números aleatórios é um conceito importante na programação, onde é necessário obter um valor imprevisível para uso em diferentes aplicativos. Isso pode ser útil para jogos, sorteios, criptografia e outras aplicações que exigem um elemento de aleatoriedade.

## Como fazer:

Para gerar um número aleatório no Clojure, podemos usar a função ```rand```. Veja um exemplo abaixo:

```Clojure
(rand) 
;; Output: 0.809462448128918

(rand 10) 
;; Output: 6.379078216751147
```

A função ```rand``` pode ser usada com ou sem um limite, onde sem um limite, ela retornará um número entre 0 e 1, e com um limite, ela retornará um número entre 0 e o limite especificado.

## Deep Dive:

Gerar números aleatórios tem sido uma necessidade na programação desde os primeiros dias da computação. Houve muitas abordagens diferentes para resolver esse problema, como a utilização de algoritmos matemáticos complexos ou geradores de números pseudoaleatórios.

O Clojure utiliza o gerador ```java.util.Random``` para gerar números aleatórios, o que garante que os números gerados sejam de alta qualidade e imprevisíveis. Isso também significa que é possível definir uma semente para o gerador para obter um resultado consistente em diferentes execuções.

Além da função ```rand```, o Clojure também possui outras funções relacionadas, como ```rand-int```, que retorna um número inteiro aleatório, e ```rand-nth```, que retorna um elemento aleatório de uma coleção.

## Veja também:

Para saber mais sobre como gerar números aleatórios no Clojure, confira a documentação oficial em [ClojureDocs](https://clojuredocs.org/clojure.core/rand) e o tutorial "Gerando números aleatórios em Clojure" no [Medium](https://medium.com/@rogeriobattimajr/gerando-numeros-aleat%C3%B3rios-em-clojure-e0a5d9010ff3).