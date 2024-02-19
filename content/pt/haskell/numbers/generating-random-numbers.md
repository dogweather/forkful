---
aliases:
- /pt/haskell/generating-random-numbers/
date: 2024-01-27 20:33:56.708979-07:00
description: "Gerar n\xFAmeros aleat\xF3rios em Haskell implica na cria\xE7\xE3o de\
  \ n\xFAmeros que s\xE3o imprevis\xEDveis segundo padr\xF5es humanos. Isso \xE9 cr\xED\
  tico em cen\xE1rios que v\xE3o desde\u2026"
lastmod: 2024-02-18 23:08:58.195032
model: gpt-4-0125-preview
summary: "Gerar n\xFAmeros aleat\xF3rios em Haskell implica na cria\xE7\xE3o de n\xFA\
  meros que s\xE3o imprevis\xEDveis segundo padr\xF5es humanos. Isso \xE9 cr\xEDtico\
  \ em cen\xE1rios que v\xE3o desde\u2026"
title: "Gera\xE7\xE3o de n\xFAmeros aleat\xF3rios"
---

{{< edit_this_page >}}

## O Que & Porquê?

Gerar números aleatórios em Haskell implica na criação de números que são imprevisíveis segundo padrões humanos. Isso é crítico em cenários que vão desde aplicações criptográficas até simulações onde o elemento de chance é necessário para modelar fenômenos do mundo real de forma precisa.

## Como fazer:

Para gerar números aleatórios em Haskell, normalmente se utiliza o pacote `random`, que faz parte da Plataforma Haskell. Aqui está um guia passo a passo:

Primeiro, certifique-se de que você tenha o pacote `random` instalado. Se não, você pode obtê-lo via Cabal ou Stack.

### Gerando um Número Aleatório

Para gerar um número aleatório simples, você pode usar a função `randomRIO`, que produz um valor aleatório dentro de um intervalo especificado.

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Número aleatório: " ++ show randomNumber
```

### Gerando uma Lista de Números Aleatórios

Gerar uma lista de números aleatórios é um pouco mais complicado, mas ainda assim direto:

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
 r <- randomRIO (1, 100)
 rs <- randomList (n-1)
 return (r:rs)

main :: IO ()
main = do
 numbers <- randomList 5
 print numbers
```

Este trecho de código cria uma função `randomList` que gera uma lista de inteiros aleatórios. Substitua `(1, 100)` pelo intervalo desejado.

## Aprofundamento

O pacote `random` de Haskell fornece um gerador de números pseudo-aleatórios (PRNG), o que significa que os números gerados não são verdadeiramente aleatórios, mas podem parecer ser aleatórios para muitas aplicações. O cerne da capacidade de geração de números aleatórios de Haskell reside na classe de tipo `RandomGen`, que abstrai diferentes métodos de geração de números aleatórios, e na classe de tipo `Random`, que inclui tipos que podem ser gerados aleatoriamente.

Historicamente, a abordagem de Haskell para a geração de números aleatórios enfatizou a pureza e a reprodutibilidade. É por isso que operações que envolvem aleatoriedade são explicitamente tratadas na monada `IO` ou exigem a passagem e atualização manual dos estados do gerador — para manter a transparência referencial.

Em certas aplicações, como criptografia, os números pseudo-aleatórios gerados pelo PRNG padrão podem não ser seguros o suficiente. Para esses casos de uso, programadores Haskell frequentemente recorrem a bibliotecas mais especializadas como `crypto-random`, que são projetadas para atender aos requisitos rigorosos de aplicações criptográficas.

Além disso, bibliotecas alternativas como `mwc-random` oferecem melhor desempenho e qualidade dos números aleatórios para simulações e outras aplicações, implementando algoritmos modernos como o Mersenne Twister.

Ao escolher uma abordagem de geração de números aleatórios em Haskell, é essencial considerar as necessidades da aplicação quanto à qualidade da aleatoriedade, desempenho e segurança para selecionar a ferramenta ou biblioteca mais apropriada.
