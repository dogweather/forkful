---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:05.074347-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Gerar números aleatórios é o processo de criar valores que não têm qualquer padrão previsível ou sequência. Programadores fazem isso para tudo, desde jogos até sistemas de segurança, garantindo elementos de surpresa ou disfarçando dados sensíveis.

## Como Fazer:
```Fish Shell
# Para gerar um número aleatório entre 0 e 999
set random_number (random 1000)
echo $random_number
```
Saída de amostra: `537`

```Fish Shell
# Para gerar um número aleatório em um intervalo específico, digamos, 50 a 100
set lower_limit 50
set upper_limit 100
set range (math "$upper_limit - $lower_limit + 1")
set random_number (math "$lower_limit + (random $range) - 1")
echo $random_number
```
Saída de amostra: `72`

## Mergulho Profundo
A geração de números aleatórios não é realmente aleatória em computadores; é o que chamamos de pseudoaleatória, dependendo de algoritmos para simular aleatoriedade. O conceito existe desde a antiguidade, mas a prática em computadores começou por volta dos anos 1950 com o avanço da teoria da computação. O Fish Shell usa o comando `random`, que gera um número pseudoaleatório com base nos números fornecidos, como mostrado nos exemplos.

Existem alternativas a considerar: algumas linguagens de programação têm suas próprias funções de geração de números aleatórios, como `rand()` em C e `random.randint()` em Python. No contexto de shells em sistemas Unix-like, utilizado pelo Fish, há também o comando `shuf` e a utilização do dispositivo especial `/dev/urandom`, que serve para necessidades mais complexas e de criptografia. A escolha pelo método vai depender do seu projeto e dos requisitos de segurança.

## Veja Também
- Documentação do Fish Shell sobre `random`: https://fishshell.com/docs/current/cmds/random.html
- Uma explicação sobre pseudoaleatoriedade: https://pt.wikipedia.org/wiki/Gerador_de_n%C3%BAmeros_pseudoaleat%C3%B3rios
- Introdução aos números aleatórios em programação: https://www.geeksforgeeks.org/random-number-generator-in-arbitrary-probability-distribution-fashion/
