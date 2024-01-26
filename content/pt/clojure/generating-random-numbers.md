---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:48:53.775476-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Gerar números aleatórios é basicamente tirar uma carta do baralho de forma cega. Programadores fazem isso para tudo, desde simulações de física a sorteios de rifas.

## How to:
Para gerar um número aleatório em Clojure, você vai brincar com a função `rand`. Quer um número entre 0 e 1? Use `rand` puro. Quer esticar essa faixa para entre 0 e 100? Multiplica por 100 e arredonda com `int`.

```Clojure
;; Número aleatório entre 0 e 1
(rand)

;; Número aleatório entre 0 e 100
(int (* 100 (rand)))
```

Rode essas linhas, e cada vez a saída é uma surpresa.

## Deep Dive:
Números "aleatórios" em programação são um truque; na verdade, são pseudoaleatórios, seguindo uma fórmula que parece aleatória. Claro, isso é suficiente para a maioria dos casos.

Historicamente, o aleatório em programação é um tema cheio de charadas e caminhos tortuosos. Em Clojure, usamos o Java underneath para dar conta do recado, graças à Java Virtual Machine (JVM).

Existem alternativas se você precisar de algo mais específico que `rand`. Bibliotecas de randomização especializadas, como `test.check`, são ótimas para testes mais sofisticados e simulações.

A implementação por baixo do capô usa um algoritmo chamado Linear Congruential Generator (LCG). Não é o mais robusto do planeta, mas para aquela rifa de fim de semana, pode confiar.

## See Also:
- A boa e velha [Clojure Docs](https://clojure.github.io/clojure/) para um mergulho mais fundo em funções randômicas.
- [test.check](https://github.com/clojure/test.check) para quando a festa de números aleatórios ficar séria.
- A história dos números pseudoaleatórios na renomada [Random.org](https://www.random.org/history/), que também oferece serviços de aleatoriedade.
