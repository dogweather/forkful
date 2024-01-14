---
title:                "Clojure: Gerando números aleatórios."
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios em Clojure?

Gerar números aleatórios é uma técnica comum em programação para criar variabilidade e imprevisibilidade em um algoritmo. Isso pode ser útil em diversas aplicações, como jogos, simulações, entre outros.

## Como fazer isso em Clojure

Em Clojure, podemos gerar números aleatórios utilizando a função `rand` do pacote `clojure.core`. Veja um exemplo simples abaixo:

```Clojure
(rand) ; gera um número aleatório entre 0 e 1
```

Podemos também especificar um intervalo utilizando parâmetros adicionais na função `rand`. Por exemplo, para gerar um número aleatório entre 1 e 10, podemos fazer o seguinte:

```Clojure
(rand 1 10) ; gera um número aleatório entre 1 e 10
```

Outra forma de gerar números aleatórios em Clojure é utilizando o pacote `java.util.Random` e sua função `nextInt`. Veja um exemplo:

```Clojure
(require '[java.util.Random :as random])

(random/nextInt 10) ; gera um número inteiro aleatório entre 0 e 9
```

## Dando um mergulho mais profundo

Para criar números aleatórios com uma distribuição específica, podemos utilizar a função `rand-nth` em conjunto com uma sequência de valores. Por exemplo, para gerar um número aleatório entre 1 e 10 com distribuição exponencial, podemos fazer o seguinte:

```Clojure
(def seq-exp (map #(Math/pow 2 %) (range))) ; cria uma sequência com valores exponenciais crescentes
(rand-nth seq-exp) ; gera um número aleatório entre 1 e 10 com distribuição exponencial
```

Além disso, podemos também gerar sequências de números aleatórios utilizando a função `repeatedly` em conjunto com `rand`. Por exemplo, para gerar uma sequência de 10 números aleatórios entre 1 e 100, podemos fazer o seguinte:

```Clojure
(take 10 (repeatedly #(rand 1 100))) ; gera uma sequência de 10 números aleatórios entre 1 e 100
```

# Veja também

- Documentação oficial da função `rand`: https://clojuredocs.org/clojure.core/rand
- Documentação oficial da função `rand-nth`: https://clojuredocs.org/clojure.core/rand-nth
- Documentação oficial da função `repeatedly`: https://clojuredocs.org/clojure.core/repeatedly
- Documentação oficial do pacote `java.util.Random`: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html