---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:36:59.125319-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Números complexos consistem de uma parte real e uma imaginária. Programadores os utilizam em campos como processamento de sinal, mecânica quântica, e sempre que o cálculo exige, porque números reais comuns simplesmente não são suficientes.

## Como fazer:
O Bash não suporta números complexos nativamente. Você frequentemente usará uma ferramenta externa como `bc` com sua opção `-l`. Aqui está como manipular números complexos no Bash:

```bash
echo "sqrt(-1)" | bc -l
```

Saída:
```bash
j
```

Multiplicação:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

Saída:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## Aprofundando
Números complexos existem desde o século 16, mas linguagens de script como o Bash não são preparadas para cálculos matemáticos como números complexos diretamente. É por isso que `bc` ou outras ferramentas como `awk` muitas vezes entram em jogo. Algumas linguagens alternativas para trabalhar com números complexos são Python com seu módulo `cmath` e MATLAB, que são ambas projetadas para funções matemáticas mais avançadas. Quanto ao Bash, é tudo uma questão de aproveitar ferramentas - `bc` usa o 'i' minúsculo para representar a unidade imaginária e suporta operações básicas como adição, subtração, multiplicação e divisão.

## Veja também
- O manual do `bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (alternativa ao MATLAB): https://www.gnu.org/software/octave/
- Módulo `cmath` do Python: https://docs.python.org/3/library/cmath.html
