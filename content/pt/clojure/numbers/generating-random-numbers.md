---
date: 2024-01-27 20:32:41.713822-07:00
description: "Gerar n\xFAmeros aleat\xF3rios na programa\xE7\xE3o \xE9 sobre criar\
  \ valores que n\xE3o podem ser previstos logicamente antes do tempo. Programadores\
  \ fazem isso por uma\u2026"
lastmod: 2024-02-19 22:05:05.263991
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios na programa\xE7\xE3o \xE9 sobre criar valores\
  \ que n\xE3o podem ser previstos logicamente antes do tempo. Programadores fazem\
  \ isso por uma\u2026"
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Gerar números aleatórios na programação é sobre criar valores que não podem ser previstos logicamente antes do tempo. Programadores fazem isso por uma variedade de razões, incluindo a geração de identificadores únicos, simulação de cenários no desenvolvimento de jogos, ou seleção de amostras aleatórias de dados para análise.

## Como fazer:

Em Clojure, a geração de números aleatórios é direta, e existem algumas funções integradas que podem ser usadas imediatamente.

Para gerar um número de ponto flutuante aleatório entre 0 (inclusivo) e 1 (exclusivo), você pode usar a função `rand`:

```Clojure
(rand)
;; Exemplo de saída: 0.7094245047062917
```

Se você precisar de um inteiro dentro de um intervalo específico, use `rand-int`:

```Clojure
(rand-int 10)
;; Exemplo de saída: 7
```

Isso lhe dá um inteiro aleatório entre 0 (inclusivo) e o número que você passa como argumento (exclusivo).

Para gerar um número aleatório dentro de um intervalo específico (não limitado a inteiros), você pode combinar `rand` com aritmética:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Uso
(rand-range 10 20)
;; Exemplo de saída: 14.857457734992847
```

Esta função `rand-range` retornará um número de ponto flutuante aleatório entre os valores `min` e `max` que você especificar.

Para cenários que exigem distribuições mais complexas ou sequências de números aleatórios onde a repetibilidade é necessária (usando sementes), pode ser necessário olhar para bibliotecas adicionais que vão além do que é integrado.

## Aprofundamento

O mecanismo subjacente para gerar números aleatórios na maioria das linguagens de programação, incluindo Clojure, tipicamente depende de um gerador de números pseudoaleatórios (PRNG). Um PRNG usa um algoritmo para produzir uma sequência de números que aproxima as propriedades de números aleatórios. Vale notar que, como eles são gerados algoritmicamente, não são verdadeiramente aleatórios, mas podem ser suficientes para a maioria dos propósitos práticos.

Nos primórdios da computação, gerar números aleatórios de alta qualidade era um desafio significante, levando ao desenvolvimento de vários algoritmos para melhorar a aleatoriedade e distribuição. Para Clojure, as funções integradas, como `rand` e `rand-int`, são convenientes para o uso diário e cobrem um amplo espectro de casos de uso comuns.

No entanto, para aplicações que requerem segurança criptográfica ou métodos de amostragem estatísticos mais complexos, os desenvolvedores de Clojure frequentemente recorrem a bibliotecas externas que oferecem PRNGs mais robustos e especializados. Bibliotecas como `clj-random` oferecem acesso a uma maior variedade de algoritmos e maior controle sobre a semeadura, o que pode ser crucial para simulações, aplicações criptográficas ou qualquer domínio onde a qualidade e previsibilidade da sequência de números aleatórios possam ter implicações significativas.

Enquanto as capacidades integradas de Clojure para gerar números aleatórios são adequadas para muitas tarefas, explorar bibliotecas externas pode oferecer percepções mais profundas e opções para aplicações sob medida ou mais críticas.
