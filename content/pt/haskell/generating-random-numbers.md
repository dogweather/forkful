---
title:                "Haskell: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que usar números aleatórios em Haskell?

Uma das características interessantes da programação funcional em Haskell é a capacidade de gerar números aleatórios. Isso pode ser útil em muitos casos, como em jogos, simulações e até mesmo em criptografia. Além disso, utilizar números aleatórios pode deixar o código mais dinâmico e divertido de se trabalhar.

## Como usar números aleatórios em Haskell

A geração de números aleatórios em Haskell envolve duas partes: a seed (semente) e a função para gerar os números. A seed é um valor inicial que é usado para criar uma sequência de números aleatórios, garantindo que os números gerados sejam diferentes a cada execução do programa.

Existem várias maneiras de gerar números aleatórios em Haskell, mas uma das mais comuns é utilizando a função `random` do módulo `System.Random`. Para isso, é necessário importar o módulo no início do código:

```Haskell
import System.Random
```

Agora, podemos definir uma seed para a geração de números aleatórios (no exemplo, utilizaremos o número 42, mas pode ser qualquer outro número):

```Haskell
gen = mkStdGen 42
```

Em seguida, podemos utilizar a função `random` para gerar um número inteiro aleatório entre 0 e 100:

```Haskell
num = randomR (0,100) gen
```

O resultado será do tipo `IO Int`, então para obter o valor do número aleatório, precisamos utilizar a função `getStdRandom`:

```Haskell
randNum = getStdRandom num
```

Agora, podemos executar o código e obter um número aleatório diferente a cada vez que o programa for rodado:

```Haskell
main :: IO ()
main = do
  randNum <- getStdRandom num
  print randNum
```

## Aprofundando na geração de números aleatórios

Além da função `random`, o módulo `System.Random` também possui outras funções úteis para trabalhar com números aleatórios em Haskell. Por exemplo, a função `randomRIO` gera um número aleatório entre dois valores especificados. Além disso, é possível gerar outros tipos de dados aleatórios, como caracteres e booleanos.

Outro aspecto importante da geração de números aleatórios em Haskell é o conceito de "monads". Sem entrar em muitos detalhes, monads são estruturas que nos permitem trabalhar com valores que podem ter efeitos colaterais, como a geração de números aleatórios. Isso é importante para garantir que cada valor gerado seja único e não dependa do contexto externo.

Em resumo, a geração de números aleatórios em Haskell pode ser uma ferramenta útil e interessante para adicionar dinamismo e imprevisibilidade aos programas. Com as diversas funções disponíveis no módulo `System.Random` e o conceito de monads, é possível criar programas mais complexos e divertidos.

## Veja também

- Documentação oficial do módulo `System.Random`: https://hackage.haskell.org/package/random
- Tutorial sobre monads em Haskell: https://en.wikibooks.org/wiki/Haskell/Understanding_monads