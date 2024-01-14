---
title:                "Python: Geração de números aleatórios"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios em Python?

Gerar números aleatórios em Python pode ser extremamente útil para diversas aplicações na área de programação. Esse recurso permite que os programadores criem simulações, jogos, ou testem algoritmos com entradas aleatórias. Além disso, também pode ser utilizado para incrementar a segurança de senhas e criptografia.

## Como gerar números aleatórios em Python

Para gerar números aleatórios em Python, podemos utilizar a biblioteca "random". É importante sempre importar essa biblioteca no início do seu código, utilizando o seguinte comando: `import random`

Agora, vamos ver alguns exemplos de como gerar diferentes tipos de números aleatórios:

```
# Para gerar um número inteiro aleatório entre 0 e 100:
aleatorio_inteiro = random.randint(0, 100)
print(aleatorio_inteiro)

# Para gerar um número real entre 0 e 1:
aleatorio_real = random.random()
print(aleatorio_real)

# Para escolher um elemento aleatório de uma lista:
lista = [1, 2, 3, 4, 5]
elemento_aleatorio = random.choice(lista)
print(elemento_aleatorio)
```

A saída desses exemplos pode variar a cada execução do código, pois os números gerados são aleatórios. É importante notar que, para gerar números aleatórios, é necessário utilizar a função `random.seed()` no início do código, preenchendo o parâmetro com um número inteiro escolhido pelo programador. Isso garante que o mesmo conjunto de números aleatórios seja gerado em cada execução do código.

## Detalhando o processo de geração de números aleatórios

A biblioteca "random" em Python utiliza um algoritmo chamado "Mersenne Twister" para gerar números aleatórios. Esse algoritmo é considerado um dos mais rápidos e eficientes para essa tarefa. Além disso, é importante ressaltar que, apesar de serem chamados de "aleatórios", os números gerados na verdade são pseudoaleatórios, ou seja, a partir de uma semente inicial, são criadas sequências de números que aparentam ser aleatórias.

Um ponto importante a se observar é que, se necessário, é possível definir uma semente específica para gerar uma sequência de números aleatórios. Isso pode ser útil em situações onde é preciso reproduzir resultados ou para fins de teste.

## Veja também

- [Documentação oficial do módulo "random" em Python](https://docs.python.org/3/library/random.html)
- [Exemplo de uso do módulo "random"](https://www.programiz.com/python-programming/examples/random-number)
- [Mais informações sobre o algoritmo "Mersenne Twister"](https://en.wikipedia.org/wiki/Mersenne_Twister)