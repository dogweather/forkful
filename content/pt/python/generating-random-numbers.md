---
title:    "Python: Gerando números aleatórios"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante na programação Python?

Gerar números aleatórios é uma habilidade essencial para qualquer programador de Python. Isso permite que você crie programas mais dinâmicos e interativos, tornando o processo de programação mais interessante e divertido.

## Como gerar números aleatórios em Python

Gerar números aleatórios em Python é surpreendentemente simples. Tudo que você precisa é da biblioteca "random".

```Python
import random

# Gerando um número aleatório entre 1 e 10
numero_aleatorio = random.randint(1, 10)
print(numero_aleatorio)

# Gerando um número flutuante aleatório entre 0 e 1
numero_flutuante = random.random()
print(numero_flutuante)

# Gerando uma lista aleatória de números entre 1 e 100
lista_aleatoria = random.sample(range(1, 100), 10)
print(lista_aleatoria)
```

Saída:

```
7
0.65236578925
[45, 12, 61, 78, 90, 2, 34, 86, 10, 47]
```

## Mergulho profundo na geração de números aleatórios em Python

Além dos métodos mais básicos de geração de números aleatórios vistos acima, a biblioteca "random" também oferece uma variedade de outras funções, como o "randrange" (gera um número aleatório dentro de um intervalo específico) e o "uniform" (gera um número flutuante aleatório entre dois limites). Também é possível manipular a semente do gerador de números aleatórios usando a função "seed", o que permite replicar sequências aleatórias em diferentes execuções do mesmo código.

Outra maneira de gerar números aleatórios é usando a biblioteca "numpy", que oferece recursos mais avançados de geração de números aleatórios, como distribuições específicas (normal, binomial, Poisson, entre outras) e arrays multidimensionais aleatórios.

## Veja também

- [Documentação da biblioteca "random" em Python](https://docs.python.org/3/library/random.html)
- [Documentação da biblioteca "numpy" em Python](https://numpy.org/doc/stable/reference/random/index.html)