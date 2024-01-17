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

## O que é e por que fazer?

Gerar números aleatórios é um processo utilizado por programadores para obter valores variados e imprevisíveis em seus códigos. Isso pode ser útil em diferentes aplicações, como jogos, simulações e criptografia.

## Como fazer:

O Python possui uma biblioteca integrada chamada ```random``` para gerar números aleatórios. Podemos utilizar a função ```random.random()``` para obter um número decimal entre 0 e 1, ou a função ```random.randint(a, b)``` para gerar um número inteiro entre os valores de ```a``` e ```b```. Aqui estão alguns exemplos:

```Python
import random

# Gerar um número decimal aleatório
num_decimal = random.random()
print(num_decimal)

# Gerar um número inteiro aleatório entre 1 e 10
num_inteiro = random.randint(1, 10)
print(num_inteiro)
```

Saída:

```
0.5673921834
7
```

## Profundando mais:

A geração de números aleatórios é uma técnica antiga e importante na computação. Na década de 1940, durante a Segunda Guerra Mundial, os cientistas precisavam de ferramentas para criptografar suas mensagens e para simular eventos em seus estudos. Foi então que nasceu o conceito de gerar números aleatórios em computadores.

Além da função ```random``` do Python, existem outras formas de gerar números aleatórios, como por meio de algoritmos ou com a utilização de hardware específico. No entanto, é importante destacar que nenhum método é totalmente aleatório, já que estamos lidando com sistemas determinísticos.

## Veja também:

- Documentação oficial do Python: https://docs.python.org/3/library/random.html
- Tutorial de geração de números aleatórios em Python: https://realpython.com/python-random/
- Artigo sobre a importância dos números aleatórios na segurança digital: https://www.thoughtco.com/why-randomization-is-important-816533