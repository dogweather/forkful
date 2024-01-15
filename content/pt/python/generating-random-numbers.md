---
title:                "Gerando números aleatórios"
html_title:           "Python: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que Gerar Números Aleatórios?

Gerar números aleatórios é uma prática comum em programação que pode ser útil em diversas situações. Eles podem ser usados para testar algoritmos, simular processos estocásticos, criar senhas seguras ou até mesmo gerar elementos em jogos.

## Como Gerar Números Aleatórios em Python

Para gerar números aleatórios em Python, utilizamos o módulo `random`. Primeiramente, importamos o módulo no início do nosso código:

```Python
import random
```

Em seguida, podemos utilizar a função `random()` para gerar um número decimal aleatório entre 0 e 1:

```Python
print(random.random())
```

Se quisermos um número inteiro aleatório entre dois valores específicos, podemos utilizar a função `randint()`, passando os valores mínimo e máximo como argumentos:

```Python
print(random.randint(1, 10))
```

Podemos também gerar uma lista com vários números aleatórios utilizando a função `sample()`, passando a lista e o número de elementos desejados como argumentos:

```Python
numeros = [1, 2, 3, 4, 5]
print(random.sample(numeros, 3))
```

## Aprofundando na Geração de Números Aleatórios

Os números gerados pela função `random()` são chamados de "pseudoaleatórios". Isso significa que, na verdade, eles são determinados por um algoritmo que tenta imitar o comportamento dos números aleatórios verdadeiros. Por essa razão, se utilizarmos a mesma semente (seed) para gerar números aleatórios, sempre obteremos a mesma sequência. Para contornar isso, podemos usar a função `seed()` para definir uma semente diferente a cada vez que o programa é executado.

Além disso, é importante ressaltar que os números gerados pelo computador não são totalmente aleatórios, já que suas ações são determinísticas. Ou seja, elas são baseadas em instruções lógicas e nem sempre podem ser consideradas completamente aleatórias. Por isso, é importante ter consciência das limitações dos números aleatórios gerados em programas.

## Veja também

- Documentação oficial do módulo `random`: https://docs.python.org/3/library/random.html
- Tutorial sobre geração de números aleatórios em Python: https://www.programiz.com/python-programming/random-numbers