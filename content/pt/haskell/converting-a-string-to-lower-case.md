---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# A arte de converter strings para minúsculas em Haskell: o quê, porquê e como?

## O quê e porquê?
Converter uma string para minúsculas é o procedimento de computação que muda todas as letras maiúsculas de uma string para minúsculas. Os programadores frequentemente utilizam essa técnica para normalizar os dados de entrada e torná-los insensíveis ao caso. 

## Como fazer:
Em Haskell, a função `toLower` do módulo `Data.Char` é frequentemente usada para converter strings para minúsculas. Vamos precisar importar esse módulo antes de utilizá-la.

```Haskell
import Data.Char (toLower)

main :: IO ()
main = do
  let str = "Hello, World!"
  putStrLn $ map toLower str
```

Este script converterá a string `"Hello, World!"` para minúsculas, e a saída será: `"hello, world!"`.

## Deep Dive
A função `toLower` tem uma história interessante, proveniente da época do ASCII, onde os caracteres maiúsculos e minúsculos são separados por um valor constante. Em Haskell, ela é implementada usando o padrão Unicode para apoiar uma ampla gama de caracteres.

Uma alternativa à função seria a aplicação direta de uma função lambda que faz a conversão:

```Haskell
import Data.Char (toLower)

main :: IO ()
main = do
  let str = "Hello, World!"
  putStrLn $ map (\c -> toLower c) str
```

A expressão `(\c -> toLower c)` é um exemplo de função lambda que toma um único caracter e o converte para minúsculas. O resultado dessa execução é o mesmo do exemplo anterior. 

Entenda que a abordagem com `map` é eficiente e idiomaticamente haskelliana. A função `map` é aplicada a cada elemento da string para resultar na conversão.

## See Also
Para ir além e se aprofundar mais em Haskell consulte as seguintes fontes:

- LYAHFGG: [Funções de alta ordem](http://learnyouahaskell.com/higher-order-functions#map)
- Hackage, Data.Char: [toLower](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html#g:12)
- SO, Haskell: [Convert String to lower or upper case](https://stackoverflow.com/questions/20368534/convert-string-to-lower-or-upper-case-in-haskell)