---
title:    "Python: Geração de números aleatórios"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios em programação?

Gerar números aleatórios é uma funcionalidade muito útil em programação, pois permite que o programa faça escolhas aleatórias e tenha um comportamento mais dinâmico e imprevisível. Além disso, é uma habilidade essencial para simulações e jogos.

## Como Gerar Números Aleatórios em Python

Existem várias maneiras de gerar números aleatórios em Python, mas a mais comum é usando a biblioteca `random`. Para começar, importe a biblioteca no início do código:

```Python
import random
```

Agora você pode utilizar as funções da biblioteca para gerar números aleatórios. Por exemplo, para gerar um número inteiro entre 1 e 10, você pode usar a função `randint()`:

```Python
aleatorio = random.randint(1,10)
print(aleatorio)
```

Este código irá imprimir um número aleatório entre 1 e 10 toda vez que for executado. Você também pode gerar números decimais usando a função `uniform()`:

```Python
aleatorio = random.uniform(1,10)
print(aleatorio)
```

Esta função irá gerar um número decimal aleatório entre 1 e 10. Você pode adaptar esses códigos de acordo com a sua necessidade, utilizando outras funções da biblioteca `random` como `choice()` e `randrange()`.

## Mergulho Profundo: Detalhes sobre Geração de Números Aleatórios

A biblioteca `random` utiliza um algoritmo chamado "Mersenne Twister" para gerar números aleatórios. Esse algoritmo é capaz de produzir uma sequência de números que parece ser completamente aleatória, mas na verdade é determinística. Isso significa que a sequência pode ser reproduzida se o mesmo ponto de partida for utilizado.

Se você quiser que a sequência seja diferente a cada vez que o programa for executado, é possível definir um ponto de partida diferente com a função `seed()`:

```Python
random.seed(1)
```

Isso irá definir o ponto de partida como 1, mas você pode utilizar qualquer outro número dessa forma.

# Veja Também

- [Documentação da biblioteca `random`](https://docs.python.org/3/library/random.html)
- [Tutorial sobre geração de números aleatórios em Python](https://machinelearningmastery.com/generate-test-datasets-python-scikit-learn/)