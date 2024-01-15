---
title:                "Gerando números aleatórios."
html_title:           "Swift: Gerando números aleatórios."
simple_title:         "Gerando números aleatórios."
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios com Swift?

Gerar números aleatórios é uma tarefa comum em muitos aplicativos e jogos. Com Swift, é muito simples e rápido, tornando a linguagem popular para essa finalidade. Neste artigo, vamos explorar como gerar números aleatórios com a última versão do Swift.

## Como fazer?

Para gerar números aleatórios em Swift, utilizamos a função `arc4random_uniform()` da biblioteca `GameplayKit`. É importante lembrar que essa função recebe um número inteiro não negativo como argumento e retorna um número aleatório entre 0 e esse número menos 1.

Para começar, importamos a biblioteca `GameplayKit` em nosso código:

```Swift
import GameplayKit
```

Em seguida, podemos chamar a função `arc4random_uniform()` passando o número desejado como argumento. Vamos gerar um número aleatório entre 1 e 10:

```Swift
let randomNumber = arc4random_uniform(10) + 1
```

O número gerado será armazenado na constante `randomNumber`. Podemos então imprimir esse número na tela usando a função `print()`:

```Swift
print("O número aleatório é \(randomNumber)")
```

Ao rodar o código, teremos um resultado como este:

```
O número aleatório é 7
```

Podemos também gerar um valor aleatório dentro de um intervalo específico. Por exemplo, se quisermos gerar um número entre 50 e 100, podemos fazer o seguinte:

```Swift
let randomNumber = arc4random_uniform(51) + 50
```

Isso nos dará um resultado entre 50 e 100. Podemos usar essa função em loops para gerar uma lista de números aleatórios ou até mesmo em jogos para determinar uma ação aleatória.

## Aprofundando

Além da função `arc4random_uniform()`, a biblioteca `GameplayKit` também possui outras funções e classes para gerar números aleatórios com mais complexidade e controle. Essas funções incluem `GKMersenneTwisterRandomSource`, `GKLinearCongruentialRandomSource` e `GKShuffledDistribution`, que permitem gerar sequências de números aleatórios mais específicas e personalizadas.

Além disso, é importante lembrar que a geração de números aleatórios não é completamente aleatória, mas sim baseada em algoritmos matemáticos. No entanto, esses algoritmos são projetados para fornecer resultados imprevisíveis, tornando a função `arc4random_uniform()` adequada para a maioria dos casos de uso.

## Veja também

Para saber mais sobre a geração de números aleatórios em Swift, acesse os links abaixo:

- Documentação oficial da função `arc4random_uniform()`: https://developer.apple.com/documentation/gameplaykit/gkarc4random_uniform
- Tutorial sobre a biblioteca `GameplayKit`: https://www.raywenderlich.com/820826-getting-started-with-gameplaykit-in-ios-10
- Explicação mais detalhada sobre o funcionamento da geração de números aleatórios: https://www.freecodecamp.org/news/what-is-random-numbers-generation/