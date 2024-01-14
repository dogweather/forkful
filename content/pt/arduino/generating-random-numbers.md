---
title:                "Arduino: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por Que

Você já se perguntou por que é importante ser capaz de gerar números aleatórios em um programa Arduino? Bem, há diversas aplicações úteis para isso! A geração de números aleatórios pode ser usada em jogos, sorteios, algoritmos de criptografia, máquinas de sorte e muito mais! É uma habilidade muito importante para quem deseja explorar e aprender mais sobre a programação em Arduino.

## Como Fazer

Para gerar números aleatórios em um programa Arduino, podemos utilizar a função ```random(min, max)```. Esta função retorna um número aleatório entre o valor mínimo (min) e o valor máximo (max) especificados. Podemos então utilizar esse número em nossas aplicações, como por exemplo:

```
Arduino random(1, 10);

// Gera um número aleatório entre 1 e 10
```

Podemos também utilizar a função ```randomSeed(seed)```, que inicializa o gerador de números aleatórios com um valor de semente (seed) específico. Isso garante que a sequência de números gerada seja diferente a cada execução do programa.

```
Arduino randomSeed(123);

// Inicializa o gerador de números aleatórios com a semente 123
```

Além disso, é possível utilizar a função ```random(min, max, seed)```, que combina as funcionalidades das duas funções anteriores. Neste caso, a semente (seed) é utilizada para gerar o primeiro número aleatório, e a cada chamada da função, um novo número é gerado baseado no anterior.

```
Arduino random(1, 10, 123);

// Gera um número aleatório entre 1 e 10, iniciando com a semente 123
// Cada nova chamada da função irá gerar um novo número baseado no anterior
```

## Diving Nutro

Para entendermos melhor como a função ```random``` gera os números aleatórios, precisamos entender um pouco sobre como os computadores trabalham com números. Na verdade, os computadores são capazes de gerar somente números pseudoaleatórios, ou seja, sequências aparentemente aleatórias, mas que são geradas por um algoritmo específico.

A função ```random``` utiliza o algoritmo Mersenne Twister, um dos mais famosos geradores de números aleatórios. Ele foi criado em 1997 e é conhecido por ter um ciclo muito longo, o que significa que a sequência gerada é praticamente indistinguível de uma sequência verdadeiramente aleatória. Porém, é importante lembrar que ainda assim, trata-se de uma sequência determinística, gerada por um algoritmo.

## Veja Também

- Documentação oficial do Arduino sobre a função ```random```: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Mais informações sobre o algoritmo Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
- Um exemplo prático de uso da função random em um jogo de dados: https://www.arduino.cc/en/tutorial/random-numbers