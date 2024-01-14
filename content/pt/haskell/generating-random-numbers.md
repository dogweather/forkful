---
title:                "Haskell: Gerando números aleatórios"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que criar números aleatórios em Haskell?

Há muitas razões para gerar números aleatórios em um programa Haskell. Pode ser útil para simulações, jogos, criptografia e muito mais. Além disso, a geração de números aleatórios também pode ser um desafio interessante para os programadores.

## Como fazer

Para gerar números aleatórios em Haskell, você pode usar o módulo "System.Random". Primeiro, importe o módulo e crie um gerador de números aleatórios com a função "mkStdGen". Em seguida, use a função "random" para gerar um número aleatório e a função "randomR" para gerar um número aleatório dentro de um intervalo específico.

```
```Haskell
import System.Random

randomGen <- mkStdGen 42 -- cria um gerador de números aleatórios com uma semente de 42
randomNumber <- random randomGen :: Int -- gera um número aleatório do tipo Int
randomNumberBetween1And10 <- randomR (1,10) randomGen :: Int -- gera um número aleatório entre 1 e 10
```

A cada vez que você executar o programa, os números gerados serão diferentes, pois eles dependem da semente do gerador de números aleatórios.

## Mergulho Profundo

Além das funções mencionadas acima, o módulo "System.Random" também possui outras funções úteis, como "randomRs" que gera uma lista de números aleatórios e "split" que divide um gerador de números aleatórios em dois. Além disso, você também pode implementar seu próprio gerador de números aleatórios usando a função "randoms" e uma função de atualização de estado.

Existem também outras bibliotecas em Haskell, como "Data.Random", que oferecem recursos mais avançados para a geração de números aleatórios.

# Veja também

- [Documentação oficial do módulo "System.Random" em Haskell](https://hackage.haskell.org/package/random-1.2.0.1/docs/System-Random.html)
- [Tutorial sobre geração de números aleatórios em Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)
- [Outras bibliotecas para geração de números aleatórios em Haskell](https://hackage.haskell.org/packages/search?terms=random&terms=haskell)