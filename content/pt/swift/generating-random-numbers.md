---
title:                "Gerando números aleatórios"
html_title:           "Swift: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Gerar números aleatórios é um recurso importante na programação, pois permite aos desenvolvedores criar aplicativos que tenham um elemento de aleatoriedade. Isso pode ser útil em jogos, sorteios ou até mesmo na geração de senhas para segurança.

## Como Fazer:

Para gerar números aleatórios em Swift, usamos a função `arc4random()`. Isso nos dá um número inteiro aleatório entre 0 e 2³²-1, que podemos manipular para atender às nossas necessidades. Aqui está um exemplo:

```
let num = arc4random()
print(num)
```
A saída será um número inteiro aleatório, por exemplo: `1021093728`.


Se quisermos um número aleatório em um intervalo específico, podemos usar o operador de módulo para obter o resto da divisão do resultado da função `arc4random()` pelo número desejado. Por exemplo, se quisermos um número entre 0 e 10, podemos fazer o seguinte:

```
let num = arc4random() % 10
print(num)
```
A saída será um número entre 0 e 9.

## Mergulho Profundo:

A função `arc4random()` é baseada no algoritmo de criptografia ARC4, que foi criado em 1987. Embora seja amplamente utilizado na programação, alguns desenvolvedores preferem usar a função `random()` do framework `Foundation`, que retorna um número `Double` entre 0.0 e 1.0.

Além disso, é importante lembrar que gerar números aleatórios não é completamente aleatório, pois é baseado em um algoritmo. Isso pode ser problemático em situações de segurança, onde números verdadeiramente aleatórios são necessários.

## Veja Também:

Para saber mais sobre a função `arc4random()`, você pode consultar a documentação oficial da Apple em: https://developer.apple.com/documentation/swift/int/1689656-arc4random. Além disso, você pode aprender mais sobre geração de números aleatórios em Swift no livro "Hacking with Swift" de Paul Hudson.