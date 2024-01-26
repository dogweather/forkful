---
title:                "Gerando números aleatórios"
date:                  2024-01-20T17:49:16.879968-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gerando números aleatórios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Gerar números aleatórios é criar valores imprevisíveis e não determinísticos. Programadores usam estes números para tudo, de jogos e simulações até segurança e análise de dados.

## Como Fazer:
```Haskell
import System.Random (newStdGen, randomRs)

-- Gerar uma lista infinita de números aleatórios entre 1 e 100:
main :: IO ()
main = do
    gen <- newStdGen
    let randomNumbers = take 10 $ randomRs (1,100) gen
    print randomNumbers
```

Saída de exemplo:
```
[35, 50, 67, 10, 42, 88, 73, 8, 21, 54]
```

## Aprofundamento
Gerar números aleatórios é um conceito antigo na computação. Haskell usa uma abordagem baseada em geradores pseudoaleatórios, o que significa que eles podem ser reproduzidos se soubermos a "semente" inicial. A função `randomRs` é apenas uma maneira de fazer isso em Haskell; há alternativas como `randomRIO`, que não requer uma semente explícita. Detalhes de implementação são críticos, pois a qualidade dos números aleatórios pode afetar a segurança e a fiabilidade do software. Haskell se baseia em uma estrutura imutável, então cada operação de geração de número aleatório precisa passar e modificar o estado do gerador.

## Veja Também
- Documentação do System.Random: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html
- "Real World Haskell", um livro que cobre mais sobre programação Haskell: http://book.realworldhaskell.org/
- Haskell Wiki sobre aleatoriedade: https://wiki.haskell.org/Random_number_generation
