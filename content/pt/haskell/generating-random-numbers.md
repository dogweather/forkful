---
title:                "Gerando números aleatórios"
html_title:           "Haskell: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## O que e por que?

Gerar números aleatórios é uma técnica comum em programação que envolve a criação de valores numéricos aleatórios para serem usados em um programa. Os programadores fazem isso para adicionar variedade e imprevisibilidade aos seus programas, tornando-os mais interessantes e úteis.

## Como fazer:

```Haskell
-- Importando o módulo de geração de números aleatórios
import System.Random

-- Gerando um número aleatório entre 1 e 10
randomNumber = randomRIO (1, 10)

-- Imprimindo o resultado
main = do
    num <- randomNumber
    print num
```

Este código importa o módulo `System.Random`, que contém funções para gerar números aleatórios. Em seguida, é usada a função `randomRIO` para gerar um número aleatório entre 1 e 10. Finalmente, o número é impresso na tela usando a função `print`.

## Detalhes avançados:

Existem várias formas de gerar números aleatórios em Haskell, incluindo o uso de funções como `random`, `randomR` e `randomIO`. Muitas vezes, o gerador de números aleatórios é inicializado com uma semente para garantir que os números gerados sejam realmente aleatórios. Além disso, é possível gerar diferentes tipos de números, como inteiros, flutuantes e booleanos.

## Veja também:

Para mais informações sobre geração de números aleatórios em Haskell, confira as seguintes fontes:

- [HaskellWiki](https://wiki.haskell.org/Random_numbers)
- [Haskell.org](https://www.haskell.org/haskellwiki/Introduction_to_QuickCheck)
- [Tutorialspoint](https://www.tutorialspoint.com/haskell/haskell_random_numbers.htm)

Agora que você sabe como gerar números aleatórios em Haskell, experimente usar essa técnica em seus próprios programas para torná-los mais interessantes e surpreendentes. Divirta-se programando!