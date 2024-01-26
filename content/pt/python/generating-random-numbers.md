---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:50:07.703027-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Gerar números aleatórios em Python é criar valores imprevisíveis. Programadores fazem isso para tarefas como jogos, simulações e escolhas aleatórias de dados.

## Como Fazer:
```Python
import random

# Gerar um número inteiro entre 1 e 10
numero_inteiro = random.randint(1, 10)
print(numero_inteiro)

# Gerar um número flutuante entre 0 e 1
numero_flutuante = random.random()
print(numero_flutuante)

# Escolher aleatoriamente um elemento de uma lista
lista = ['maçã', 'banana', 'cereja']
escolha = random.choice(lista)
print(escolha)
```

Saída de exemplo:
```
7
0.320394586
banana
```

## Mergulho Profundo
Desde o surgimento dos computadores, gerar números aleatórios foi essencial. Inicialmente, utilizavam-se métodos baseados em fenômenos físicos, mas com avanços, algoritmos como o Mersenne Twister tornaram-se padrões devido à sua eficiência e periodicidade longa. Contudo, é vital saber que as funções de aleatoriedade em muitas linguagens de programação, incluindo Python, não são verdadeiramente aleatórias; elas são pseudoaleatórias, usando sementes ('seeds') para produzir sequências previsíveis se a semente for conhecida.

Alternativas ao `random` padrão incluem `numpy.random` para arrays grandes e `secrets` para criptografia e segurança, onde a verdadeira aleatoriedade é crucial. Outro ponto importante é garantir a reproducibilidade em testes ou simulações, por meio da função `random.seed()`, que configura a semente.

## Veja Também
- Documentação oficial do módulo `random`: https://docs.python.org/3/library/random.html
- Uma discussão sobre aleatoriedade e sementes: https://stackoverflow.com/questions/22639587/random-seed-what-does-it-do
- Documentação do módulo `secrets` para uso em segurança: https://docs.python.org/3/library/secrets.html
- Informações sobre o algoritmo Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
