---
title:                "Swift: Gerando números aleatórios"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Porque:
Gerar números aleatórios é uma habilidade essencial para qualquer programador. Através do uso dessa técnica, podemos criar jogos divertidos, testar algoritmos e simular situações do mundo real. Além disso, é uma ferramenta útil para aumentar a aleatoriedade em nossos projetos.

## Como Fazer:
A linguagem Swift possui uma função embutida para gerar números aleatórios, chamada `random`. Com ela, podemos gerar um número aleatório entre 0 e 1, bastando apenas usar `random()` em nosso código. Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, basta modificar o código para `random(in: 1...10)`.

Outra opção é utilizar a função `arc4random_uniform()` que gera um número inteiro aleatório dentro de um intervalo. Por exemplo:

```Swift
let randomNum = arc4random_uniform(100) // gera um número aleatório entre 0 e 99
```

Podemos usar essa função para criar jogos simples, como adivinhação de número, ou até mesmo para embaralhar listas de dados.

## Mergulho Profundo:
Além das funções mencionadas, a linguagem Swift tem uma classe específica para gerar valores aleatórios: `RandomNumberGenerator`. Essa classe, juntamente com seu respectivo protocolo `RandomNumberGeneratorProtocol`, permite um controle mais preciso sobre a geração de números aleatórios.

Além disso, é importante lembrar que gerar números verdadeiramente aleatórios é impossível em um computador. Por isso, é comum usar um valor chamado de "seed" para garantir uma sequência de números pseudoaleatórios. Podemos definir essa seed utilizando a função `srand48(Int)`, que define a semente a ser usada pela função `random()`.

## Veja Também:
- [Documentação Oficial da Função Random em Swift](https://developer.apple.com/documentation/swift/random)
- [Tutoriais sobre Geração de Números Aleatórios em Swift](https://www.raywenderlich.com/149329/swift-generate-random-data-numbers-in-swift-4)
- [Explicação sobre a Diferença entre Números Aleatórios Verdadeiros e Pseudoaleatórios](https://www.quora.com/What-is-the-difference-between-true-random-and-pseudorandom-number-generators)