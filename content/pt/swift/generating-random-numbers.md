---
title:    "Swift: Geração de números aleatórios"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?
Muitas vezes, em programação, precisamos gerar números aleatórios para simular situações ou para criar uma variedade de opções. Isso é especialmente útil em jogos e aplicações de sorteio, mas também pode ser aplicado em outras áreas de desenvolvimento.

## Como fazer
Existem algumas maneiras de gerar números aleatórios em Swift. A seguir, mostraremos um exemplo simples e eficaz utilizando a função `arc4random_uniform()`.

```
// Importe a biblioteca de arc4random
import Foundation

// Gera um número aleatório entre 1 e 100
let numero = Int(arc4random_uniform(100))

// Imprime o número gerado
print(numero)

// Saída de exemplo: 54
```

Este método retorna um número inteiro aleatório dentro do intervalo especificado. No exemplo acima, utilizamos a função `Int()` para garantir que o número gerado seja do tipo inteiro.

## Mergulho profundo
A função `arc4random_uniform()` utiliza o gerador de números pseudorandômicos do sistema operacional, que é considerado seguro e eficiente para a maioria dos casos. No entanto, se você precisar de uma geração mais precisa, pode utilizar a estrutura `Random` presente na biblioteca padrão do Swift.

```
// Importe a biblioteca padrão do Swift
import Foundation

// Gera um número aleatório entre 1 e 100
let numero = Int.random(in: 1...100)

// Imprime o número gerado
print(numero)

// Saída de exemplo: 82
```

Diferente da função `arc4random_uniform()`, a estrutura `Random` utiliza um algoritmo próprio para gerar números aleatórios de forma mais precisa. Além disso, é possível especificar o tipo de dado desejado e até mesmo uma semente para gerar números repetíveis.

## Veja também
- [Documentação oficial da Apple sobre geração de números aleatórios em Swift](https://developer.apple.com/documentation/swift/random)
- [Tutorial de geração de números aleatórios em Swift](https://www.hackingwithswift.com/read/38/5/challenge-how-to-generate-a-random-number)