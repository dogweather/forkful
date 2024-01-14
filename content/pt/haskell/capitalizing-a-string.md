---
title:    "Haskell: Capitalizando uma string"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que

Ao escrever códigos em Haskell, muitas vezes nos deparamos com a necessidade de manipular strings. Um problema comum é a necessidade de capitalizar uma string, ou seja, transformar todas as letras minúsculas em maiúsculas. Neste tutorial, vamos aprender como fazer isso de forma simples e eficiente.

## Como Fazer

A função `toUpper` da biblioteca `Data.Char` pode ser usada para capitalizar uma string. Esta função recebe um caractere como entrada e retorna o mesmo caractere em maiúsculo. Para aplicar essa função em uma string, podemos usar a função `map`, que aplica uma função a cada elemento de uma lista. Veja o exemplo abaixo:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str
```

No código acima, importamos a função `toUpper` e a aplicamos na string de entrada usando a função `map`. Para testar a função, podemos usar o seguinte código:

```Haskell
main = do
  let str = "haskell"
  putStrLn (capitalize str)
```

A saída será `HASKELL`, que é a mesma string com todas as letras maiúsculas.

## Deep Dive

A função `toUpper` é definida da seguinte forma na biblioteca `Data.Char`:

```Haskell
toUpper :: Char -> Char
toUpper c
  | isAsciiLower c = chr (ord c + (ord 'A' - ord 'a'))
  | otherwise = c
```

Essa função verifica se o caractere de entrada `c` é minúsculo usando a função `isAsciiLower`. Se for, então é adicionado o valor da diferença entre os códigos ASCII de `c` e `A` ao código ASCII de `c`, resultando no código do caractere em maiúsculo. Caso contrário, o próprio caractere é retornado.

## Veja Também

- [Data.Char documentation](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Haskell - Manipulando Strings](http://haskell.tailorfontela.com.br/strings)
- [Funções `map` e `toUpper` em Haskell](https://www.youtube.com/watch?v=5gloNnJ8IOg)