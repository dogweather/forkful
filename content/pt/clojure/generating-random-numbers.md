---
title:                "Clojure: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Clojure?

A geração de números aleatórios é uma tarefa comum em muitos projetos de programação. Pode ser necessário para simular dados, testar algoritmos ou até mesmo criar jogos. Felizmente, em Clojure, gerar números aleatórios é uma tarefa fácil e simples de ser realizada. Nesta postagem, vamos explorar como gerar números aleatórios em Clojure e aprender mais sobre o processo por trás dessa tarefa.

## Como fazer

Para gerar números aleatórios em Clojure, podemos usar a função `rand` do pacote `clojure.core` juntamente com a função `rand-int`. A função `rand` retorna um número aleatório entre 0 e 1, enquanto a função `rand-int` retorna um número inteiro aleatório dentro de um intervalo especificado.

```
```Clojure
(require '[clojure.core :refer [rand]])

; gerando um número aleatório entre 0 e 1
(rand)

; gerando um número aleatório entre 10 e 20
(rand-int 10 20)
```
```

A função `rand-int` também pode ser usada para gerar uma sequência de números aleatórios, basta especificar o tamanho da sequência e o intervalo de números.

```
```
```Clojure
; gerando uma sequência de 5 números aleatórios entre 1 e 10
(rand-int 5 1 10)
```
```

Podemos ver o resultado da nossa geração de números aleatórios nos blocos de código acima. Cada vez que executamos o código, obtemos um resultado diferente, pois os números gerados são realmente aleatórios.

## Deep Dive

Por trás da tarefa aparentemente simples de gerar números aleatórios em Clojure, há um processo matemático complexo. Os números são gerados com base em um algoritmo conhecido como *sistema congruente linear*, que usa uma função de mapeamento para gerar um número aleatório a partir de um número anteriormente gerado. Isso é combinado com um chamado *algoritmo de Borland*, que ajuda a garantir que os números gerados sejam verdadeiramente aleatórios e uniformemente distribuídos.

## Veja também

Aqui estão alguns links para outros recursos úteis sobre a geração de números aleatórios em Clojure:

- [Documentação oficial do Clojure sobre a função `rand`](https://clojuredocs.org/clojure.core/rand)
- [Explicação mais detalhada sobre o sistema congruente linear e o algoritmo de Borland](https://www.math.ubc.ca/~feldman/m321/linear.pdf)
- [Exemplo prático de como usar a função `rand-int` para gerar senhas aleatórias](https://www.baeldung.com/clojure-random-password)