---
title:    "Arduino: Geração de números aleatórios"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios no Arduino?

Gerar números aleatórios pode ser muito útil em projetos com Arduino, principalmente para adicionar uma camada extra de imprevisibilidade e aleatoriedade. Isso pode ser útil em jogos, sorteios, simulações, entre outros. Além disso, é uma boa forma de explorar e entender melhor as capacidades do Arduino.

## Como gerar números aleatórios no Arduino

Gerar números aleatórios no Arduino é muito simples e pode ser feito utilizando a função `random(min,max)` do Arduino. Essa função aceita dois parâmetros: o valor mínimo desejado e o valor máximo desejado. Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, podemos utilizar o código a seguir:

```Arduino
int numero_aleatorio = random(1,10);
Serial.println(numero_aleatorio);
```

Isso irá gerar um número aleatório e imprimi-lo no monitor serial do Arduino.

## Aprofundando no tópico

Gerar números aleatórios não é uma tarefa tão simples quanto parece. Na verdade, não existe uma forma de gerar números absolutamente aleatórios em um sistema digital. O que fazemos é utilizar algoritmos e técnicas para gerar números que se comportem de forma aleatória e imprevisível na maioria das situações.

No Arduino, o algoritmo utilizado pela função `random` é o algoritmo linear congruencial. Ele funciona gerando sucessivas iterações a partir de uma equação matemática. Porém, essa função possui limitações e pode gerar sequências repetitivas de números em alguns casos. Para contornar esse problema, existem outras técnicas e algoritmos que podem ser utilizados, como o algoritmo de Mersenne Twister.

## Veja também

- [Tutorial oficial do Arduino sobre números aleatórios](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Artigo sobre algoritmos para geração de números aleatórios](https://www.freecodecamp.org/news/how-to-generate-random-numbers-in-javascript/)