---
title:                "Swift: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios é importante na programação Swift?

Gerar números aleatórios é uma tarefa bastante comum em programação, pois muitas vezes precisamos de dados imprevisíveis para testar nossos programas ou criar elementos dinâmicos. Na Swift, existem várias maneiras de gerar e manipular números aleatórios, e neste artigo vamos explorar algumas delas.

## Como fazer?

A forma mais simples de gerar números aleatórios na Swift é utilizando a função `random()`, que retorna um número aleatório entre 0 e 1 em formato `Double`. Veja um exemplo abaixo:

```Swift
let randomNum = Double.random()
print(randomNum)
```
Este código irá gerar um número aleatório e imprimi-lo no console, sendo um resultado como "0.835965173456346".

Se você precisa gerar um número inteiro, pode utilizar a função `Int.random(in: )`, passando o intervalo desejado como parâmetro. Por exemplo:

```Swift
let randomInt = Int.random(in: 1...10) // irá gerar um número inteiro entre 1 e 10
```
Você também pode gerar um número aleatório entre dois números inteiros específicos utilizando `Int.random(between: , and: )`. Veja um exemplo:

```Swift
let randomYear = Int.random(between: 2000, and: 2020) // irá gerar um ano aleatório entre 2000 e 2020
```
Além disso, a Swift possui uma estrutura chamada `RandomNumberGenerator` que permite maior controle sobre a geração de números aleatórios. Você pode criar uma instância desta estrutura e utilizar seus métodos para gerar números aleatórios, como `next()` e `next(upperBound: )`.

## Aprofundando-se mais

A fim de gerar números verdadeiramente aleatórios, é importante entender que, na verdade, nenhum algoritmo é capaz de produzir resultados completamente aleatórios. O que acontece é que os números gerados por esses algoritmos são "pseudo-aleatórios", pois seguem uma sequência previsível, mas aparentemente aleatória. Esta sequência é chamada de "seed" e, geralmente, é baseada no tempo ou em alguma entrada do usuário.

Na Swift, a função `random()` usa o relógio interno do sistema como semente, enquanto as outras funções mencionadas anteriormente usam o padrão `PCG32`, que é um gerador de números aleatórios de alta qualidade.

## Veja também

- [Documentação oficial da Swift sobre geração de números aleatórios](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID223)
- [Blog da Ray Wenderlich sobre geração de números aleatórios na Swift](https://www.raywenderlich.com/4378-how-to-generate-random-numbers-in-swift#toc-anchor-004)
- [Tutorial em vídeo sobre uso de RandomNumberGenerator na Swift](https://www.youtube.com/watch?v=QFHEEIT_W1U)