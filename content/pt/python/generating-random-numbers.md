---
title:                "Python: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios em Python?

Gerar números aleatórios é uma tarefa muito comum na programação e pode ser útil em diversas situações, como em jogos, simulações e testes de software. Com o uso do Python, podemos facilmente criar algoritmos para gerar números aleatórios de acordo com as necessidades do nosso programa.

## Como fazer

Para gerar números aleatórios em Python, precisamos primeiro importar o módulo "random". Em seguida, podemos utilizar as funções deste módulo para gerar diferentes tipos de números aleatórios.

Veja um exemplo de código utilizando a função "random()" para gerar um número aleatório entre 0 e 1:

```Python
import random
print(random.random())
```

A saída deste código pode ser algo como: 0.45829643230384574.

Podemos utilizar a função "randint(a, b)" para gerar um número inteiro aleatório entre dois valores a e b, como neste exemplo que gera um número entre 1 e 100:

```Python
import random
print(random.randint(1,100))
```

A saída será um número inteiro, como por exemplo: 67.

Também é possível gerar uma lista de números aleatórios utilizando a função "sample(lista, k)", onde "lista" é a lista em que serão gerados os números aleatórios e "k" é a quantidade de números a serem gerados. Veja o exemplo a seguir:

```Python
import random
print(random.sample(range(1,50), 5))
```

A saída será uma lista de 5 números aleatórios entre 1 e 50, como por exemplo: [12, 34, 17, 45, 21].

## Mergulho profundo

A geração de números aleatórios não é uma tarefa simples e requer conhecimento de diversos conceitos matemáticos e estatísticos. No caso do Python, ele utiliza o algoritmo Mersenne Twister para a geração de números pseudoaleatórios. Isso significa que os números gerados não são totalmente aleatórios, mas sim seguindo um padrão matemático determinístico. Isso é importante para que possamos reproduzir os mesmos resultados quando desejarmos.

Além disso, existem diversas maneiras de controlar a geração de números aleatórios em Python, como fixar uma seed (semente) para termos sempre os mesmos resultados, embaralhar uma lista de números já existentes, entre outras técnicas mais avançadas.

# Veja também

- Documentação oficial do módulo "random" em Python: https://docs.python.org/3/library/random.html
- Tutorial sobre geração de números aleatórios em Python: https://www.geeksforgeeks.org/random-numbers-in-python/
- Vídeo explicando o algoritmo Mersenne Twister utilizado pelo Python: https://www.youtube.com/watch?v=_CAfaUlJ2-Y