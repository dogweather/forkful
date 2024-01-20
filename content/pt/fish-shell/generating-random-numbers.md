---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O quê e por quê?

Gerar números aleatórios é a prática de criar sequências de números que não têm padrão discernível. Os programadores fazem isso para qualquer situação em que a aleatoriedade seja necessária, como para simulações, criptografia e jogos.

## Como fazer:

Aqui está um exemplo de como você pode gerar um número aleatório entre 1 e 100 no Fish Shell:

```Fish Shell
set -l range_start 1
set -l range_end 100
math (random % ($range_end + 1 - $range_start)) + $range_start"
```

Quando você executa o código acima, o shell retorna um número aleatório no intervalo especificado. Por exemplo, poderia retornar `78` ou `21`.

## Aprofundando

O conceito de números aleatórios tem uma longa história na computação, mas o que é importante saber é que os números gerados por computadores não são verdadeiramente aleatórios. Eles são determinados por algoritmos e, portanto, são chamados de "pseudo-aleatórios". 

Existem alternativas no Fish Shell para gerar números aleatórios, por exemplo, usando o comando `jot -r`.

```Fish Shell
jot -r 1 1 100
```

No entanto, a função `math` com o módulo `random` é geralmente mais rápida e preferida para a geração de números aleatórios.

## Veja também:

- [Fish Shell Documentation](https://fishshell.com/docs/current/commands.html#random)
- [StackOverflow: How to generate a random number in Fish?](https://stackoverflow.com/questions/55656492/how-to-generate-a-random-number-in-fish)
- [Unix & Linux Stack Exchange: How to get a random number in Fish Shell?](https://unix.stackexchange.com/questions/140750/how-do-you-get-a-random-number-in-fish)