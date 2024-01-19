---
title:                "Gerando números aleatórios"
html_title:           "C: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Gerando números aleatórios em Python

## O Que & Porquê?

Gerar números aleatórios é uma atividade comum em programação, onde um valor numérico diferente é produzido a cada execução de um programa. Isso é útil para criação de testes de software, modelagem e simulações, além de jogos digitais onde a imprevisibilidade aumenta a diversão.

## Como se faz:

Aqui estão alguns exemplos que mostram como gerar números aleatórios em Python. Estes scripts usam o módulo `random` que vem com Python.

Gerando um número float aleatório entre 0.0 e 1.0.

```Python
import random
print(random.random())
```

Saída de amostra: `0.5672034741778504`

Gerando um número inteiro aleatório entre 1 e 10.

```Python
import random
print(random.randint(1, 10))
```

Saída de amostra: `6`

## Imersão Profunda:

A geração de números aleatórios tem suas raízes na teoria das probabilidades e tem aplicações notáveis em áreas como estatísticas e criptografia. Python usa o método Mersenne Twister como a principal maneira de gerar números pseudorandom. Embora este método seja suficiente para a maioria das aplicações, existem alternativas como `numpy.random` que podem ser mais adequadas para aplicações científicas ou matemáticas.

Discussões sobre o quão "aleatórios" são esses números podem se tornar bastante complexas, envolvendo conceitos como "entropia" e "aleatoriedade verdadeira".

## Veja Também:

- Documentação oficial Python para o módulo random: [Python Doc](https://docs.python.org/pt-br/3/library/random.html)
- Para números aleatórios sofisticados: [Numpy library](https://numpy.org/)
- Detalhes mais técnicos sobre a geração de números aleatórios: [Wikipedia article](https://en.wikipedia.org/wiki/Random_number_generation)

Agora você deve ter um bom entendimento de como gerar números aleatórios em Python. Divirta-se codificando!