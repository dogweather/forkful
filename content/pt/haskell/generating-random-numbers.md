---
title:                "Gerando números aleatórios."
html_title:           "Haskell: Gerando números aleatórios."
simple_title:         "Gerando números aleatórios."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios?

Gerar números aleatórios é uma das funcionalidades mais úteis e interessantes da programação. Seja para criar jogos, simulações ou testar algoritmos, os números aleatórios são amplamente utilizados na maioria das linguagens de programação. Em Haskell, isso não é diferente. Aqui você aprenderá como gerar números aleatórios de forma simples e eficiente.

## Como fazer em Haskell

Para gerar números aleatórios em Haskell, precisamos usar a biblioteca `System.Random`. Essa biblioteca contém funções para gerar valores pseudoaleatórios (não verdadeiramente aleatórios, mas suficientemente aleatórios para a maioria dos casos).

Primeiro, importamos a biblioteca `System.Random`:

```Haskell
import System.Random
```

Em seguida, usamos a função `randomR` para gerar um número inteiro entre dois valores fornecidos:

```Haskell
randomNumber :: Int
randomNumber = randomR (1,10)
```

No exemplo acima, geramos um número aleatório entre 1 e 10. Podemos modificar esses valores de acordo com as nossas necessidades.

Se quisermos gerar um número aleatório dentro de uma lista de valores, podemos usar a função `randomElem` da biblioteca `System.Random`:

```Haskell
myList = [1, 2, 3, 4, 5]
randomElem :: [Int]
randomElem = randomElem myList
```

No exemplo acima, geramos um número aleatório dentro da lista `myList` e o armazenamos na variável `randomElem`.

Além disso, é possível gerar números aleatórios com diferentes tipos de dados, como `Double`, `Bool` e `Char`. Basta declarar o tipo de dado desejado antes da função `randomR` ou `randomElem`.

## Mergulho mais profundo

Os números aleatórios gerados por funções como `randomR` e `randomElem` são baseados em um número inicial chamado "seed" (semente). Isso significa que, ao reiniciar o programa, os mesmos números aleatórios serão gerados novamente. Para evitar isso, podemos usar a função `newStdGen` para gerar uma nova semente aleatória a cada vez que o programa é executado:

```Haskell
newSeed :: IO Int
newSeed = getStdRandom $ random (1,10)
```

Podemos também utilizar a função `randomIO` para gerar números aleatórios em qualquer ponto do programa, sem precisar declarar uma semente:

```Haskell
randomNumber :: IO Int
randomNumber = randomIO
```

É importante lembrar que, ao utilizar essas funções, estamos gerando números pseudoaleatórios, ou seja, não verdadeiramente aleatórios. Se precisarmos de números verdadeiramente aleatórios, é necessário usar bibliotecas específicas.

# Veja também

- Documentação da biblioteca System.Random: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html
- Tutorial sobre geração de números aleatórios em Haskell: https://www.haskell.org/tutorial/randomness.html