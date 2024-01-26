---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:53.630970-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que É e Porquê?
Gerar números aleatórios é uma forma de criar valores imprevisíveis no seu programa. Programadores usam isso para tudo – de jogos a simulações e segurança, onde a aleatoriedade pode ser crítica.

## Como Fazer:

```Swift
import Foundation

// Gerar um número aleatório entre 0 e 999
let randomNumber = Int.random(in: 0..<1000)
print(randomNumber)

// Gerar um número aleatório de ponto flutuante
let randomFloat = Float.random(in: 0..<1)
print(randomFloat)

// Exemplo de uso em um jogo para escolher aleatoriamente um personagem
let characters = ["Alice", "Bob", "Charlie"]
if let randomCharacter = characters.randomElement() {
    print("Personagem selecionado: \(randomCharacter)")
}
```

## Mergulho Profundo

Antigamente, gerar números aleatórios em computadores dependia de algoritmos chamados geradores de números pseudoaleatórios (PRNGs), porque a verdadeira aleatoriedade é difícil de conseguir em máquinas determinísticas. No Swift, usamos APIs que encapsulam PRNGs como o ARC4 ou mt19937, mas o padrão atual é utilizar o `arc4random` para facilidade e segurança.

Alternativas modernas ao PRNG padrão incluiriam o uso de `SecRandomCopyBytes` para maior segurança em aplicações críticas, pois ele usa fontes de entropia mais robustas.

A implementação de números aleatórios na standard library do Swift oferece flexibilidade, permitindo escolher manipular o intervalo dos números e o tipo de dados – sejam inteiros, floats, ou até mesmo valores personalizados.

## Veja Também

- "Swift Programming: The Big Nerd Ranch Guide" por Matthew Mathias e John Gallagher, que tem um capítulo dedicado a números aleatórios.
- "Algorithms" por Robert Sedgewick e Kevin Wayne, que explora PRNGs e outras funções como parte da ciência da computação algorítmica.
