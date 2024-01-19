---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Encontrando o tamanho de uma string em Haskell

## O que é e por quê?

Encontrar o tamanho de uma string significa determinar o número de caracteres que ela contém. Como programadores, fazemos isso quando precisamos saber quantos caracteres um usuário inseriu, ou quando estamos trabalhando com análises de texto.

## Como fazer:

Para encontrar o tamanho (ou comprimento) de uma string em Haskell, você pode usar a função `length`.

```Haskell
tamanho :: String -> Int
tamanho str = length str
```

Para testar, você pode utilizar essa função no interprete do Haskell.

```Haskell
ghci> tamanho "Ola, mundo!"
13
```

Este resultado indica que a string "Ola, mundo!" tem 13 caracteres.

## Mergulhando fundo

A função `length` em Haskell tem sua raiz no cálculo Lambda, um conceito que é fundamental para a programação funcional e para Haskell em si. Por estar usando listas, essa função precisa percorrer toda a lista (ou seja, a string) para contar seus elementos, o que implica em um tempo de execução de O(n).

Há outras maneiras de encontrar o tamanho de uma string em Haskell. Você pode usar recursão direta para implementar uma função de comprimento por conta própria. No entanto, `length` é mais comumente usado por causa de sua simplicidade e eficiência.

Além disso, é importante lembrar que strings em Haskell são listas de caracteres. Portanto, o tamanho de uma string é o número de caracteres que ela contém, e não o número de bytes.

## Veja também

Você pode aprender mais sobre strings e listas em Haskell nos seguintes recursos:

1. LYAHFGG (Learn You a Haskell For Great Good) sobre [listas](http://learnyouahaskell.com/starting-out#tuples)
2. Real World Haskell: [Strings e listas de caracteres](http://book.realworldhaskell.org/read/using-typeclasses.html)
3. A implementação da função [length](https://hackage.haskell.org/package/base-4.15.0.0/docs/src/GHC.List.html#length) na biblioteca padrão de Haskell
4. Se você é novo em Haskell, você pode achar útil esse tutorial sobre [setup de ambiente e sintaxe básica](http://learnyouahaskell.com/introduction).