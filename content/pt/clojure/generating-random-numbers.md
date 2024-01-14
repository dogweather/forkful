---
title:    "Clojure: Gerando Números Aleatórios"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que Gerar Números Aleatórios?

A geração de números aleatórios é uma ferramenta útil em muitas áreas da programação, desde jogos eletrônicos até algoritmos de criptografia. Com a capacidade de gerar números aleatórios, podemos criar aplicações mais dinâmicas e imprevisíveis, aumentando a complexidade e diversão do processo de desenvolvimento.

## Como Fazer

Para gerar números aleatórios em Clojure, vamos usar a função `rand`, que retorna um número aleatório entre 0 e 1. Podemos multiplicar o resultado por um número para expandir o intervalo de valores possíveis. Por exemplo, para gerar um número aleatório entre 1 e 10, podemos multiplicar o resultado de `rand` por 10 e então arredondar para baixo com a função `Math/floor`.

```Clojure
(Math/floor (* (rand) 10))
```

Isso irá gerar um número inteiro entre 0 e 9. Você pode experimentar diferentes valores para expandir ainda mais o intervalo de valores possíveis.

## Deep Dive

A função `rand` usa o gerador de números pseudorandomicos do Java, que é baseado no algoritmo de congruência linear. Isso significa que os números gerados não são verdadeiramente aleatórios, mas sim uma sequência de números previsíveis. No entanto, essa sequência é tão complexa e caótica que pode ser considerada suficientemente aleatória para a maioria dos casos de uso.

Se precisarmos de uma geração mais precisa e realmente aleatória, podemos utilizar a biblioteca `clojure.math.numeric-tower` e a função `random-sample` para gerar uma sequência de números a partir de uma distribuição uniforme. Essa função usa o gerador de números aleatórios do sistema operacional e é mais indicada para aplicações que envolvem criptografia ou simulações científicas.

## Veja Também

- [Documentação do Clojure sobre a função `rand`](https://clojure.org/reference/numbers#var_rand)
- [Pergunta e resposta sobre geração de números aleatórios no Stack Overflow](https://stackoverflow.com/questions/27086174/generate-random-number-in-clojure)