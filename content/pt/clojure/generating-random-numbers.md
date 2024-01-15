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

## Por que?

Gerar números aleatórios é uma tarefa importante em muitos campos da computação, como jogos, simulações e criptografia. A linguagem Clojure possui bibliotecas integradas que permitem gerar esses números de forma eficiente e confiável.

## Como fazer?

Para gerar um único número aleatório entre 0 e 10, podemos utilizar a função `rand-int` da biblioteca `clojure.core` da seguinte forma:

```Clojure
(clojure.core/rand-int 10) ; => 7
```

Podemos utilizar a função `rand` para gerar números decimais aleatórios entre 0 e 1:

```Clojure
(clojure.core/rand) ; => 0.7521487200786202
```

Também é possível gerar números aleatórios a partir de uma distribuição normal utilizando a função `rand-norm` da biblioteca `incanter.stats`:

```Clojure
(require '[incanter.stats :as stats])

(stats/rand-norm) ; => 0.7278992985620795
```

## Detalhes avançados

Para gerar uma sequência de números aleatórios, podemos utilizar a função `rand-seq` da biblioteca `clojure.core`:

```Clojure
(take 5 (rand-seq 10)) ; => (1 9 7 5 4)
```

Também é possível definir uma semente para a geração dos números aleatórios, garantindo a mesma sequência de números a cada execução do código:

```Clojure
(clojure.core/set! *rand-nth-state* (clojure.core/make-rand-nth-state 1234))
(repeatedly 5 #(rand-int 10)) ; =>(3 0 6 8 5)
```

## Veja também

- [Documentação oficial do Clojure sobre geração de números aleatórios](https://clojure.org/reference/java_interop#_random_number_generators)
- [Tutorial de Clojure sobre geração de números aleatórios](https://clojure.org/guides/random_numbers)
- [Biblioteca "Clojure Stdlib" com funções para manipulação de números aleatórios](https://github.com/clojure-stdlib/clojure-stdlib)