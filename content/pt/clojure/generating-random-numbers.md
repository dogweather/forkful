---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Gerar números aleatórios é um processo de produção de números que não seguem um padrão perceptível ou previsível. Programadores fazem isso para uma variedade de propósitos, como criar dados de teste, simulações e jogos.

## Como Fazer:
Aqui está um exemplo de como gerar um número aleatório em Clojure:

```clojure
(defn numero-aleatorio []
    (rand))
```

Ao executar `(numero-aleatorio)`, você vai receber um número aleatório entre 0.0 (inclusivo) e 1.0 (exclusivo).

Se quiser gerar um número inteiro aleatório num intervalo, você pode usar a função `rand-int`. Aqui está um exemplo de como gerar um número inteiro aleatório até 10:

```clojure
(defn numero-aleatorio-ate-10 []
    (rand-int 10))
```

Ao executar `(numero-aleatorio-ate-10)`, você vai receber um número inteiro aleatório de 0 (inclusivo) a 10 (exclusivo).

## Aprofundamento
A geração de números aleatórios tem uma longa história na computação. Muitos algoritmos foram criados para esse propósito, mas a abordagem de Clojure é simples e eficaz.

Existem outras maneiras de gerar números aleatórios em Clojure, como o uso do gerador de números aleatórios java.util.Random diretamente, mas `rand` e `rand-int` geralmente são suficientes para a maioria dos usos.

Ao usar `rand` e `rand-int`, você está tecnicamente gerando números pseudoaleatórios. Eles são determinísticos e podem ser reproduzidos se você souber a "semente" inicial. Se você precisar de números verdadeiramente aleatórios para criptografia ou outros usos sensíveis, terá que usar uma fonte diferente.

## Veja Também

1. Clojure - Funções matemáticas : [https://clojuredocs.org/clojure.core/rand](https://clojuredocs.org/clojure.core/rand)
3. Geradores de números pseudoaleatórios : [https://pt.wikipedia.org/wiki/Gerador_de_números_pseudoaleatórios](https://pt.wikipedia.org/wiki/Gerador_de_números_pseudoaleatórios).