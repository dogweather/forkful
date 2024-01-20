---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é & Porquê?

Concatenar strings é o processo de juntar, ou ligar, duas ou mais strings, formando uma única string. Programadores fazem isso para manipular ou reformatar dados de texto.

## Como fazer:

Em Haskell, você pode usar o operador `++` para concatenar strings. Vamos a um exemplo:

```Haskell
main = do
  let str1 = "Olá "
  let str2 = "mundo!"
  let str3 = str1 ++ str2
  putStrLn str3
```

Neste código, `str3` irá conter "Olá mundo!". Quando executar `putStrLn str3`, irá imprimir "Olá mundo!" na tela.

## Mergulho Profundo:

A concatenação de strings é um conceito básico que existe desde o início da programação e está presente em todas as linguagens de programação.

No caso específico de Haskell, uma alternativa a `++` é a função `concat`. Esta função é útil quando temos uma lista de strings que queremos juntar em uma única string.

E sobre a implementação: Em Haskell, uma string é uma lista de caracters. Portanto, concatenar strings é o mesmo que concatenar listas.

Aqui está um exemplo usando `concat`:

```Haskell
main = do
  let strList = ["Olá ", "mundo!"]
  let str = concat strList
  putStrLn str
```

Esta impressão resultará em "Olá mundo!".

## Ver também:

- Guia de Cordas Haskell: hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html
- Livro Learn You a Haskell for Great Good: learnyouahaskell.com/starting-out#an-intro-to-lists
- Documentação de String na Biblioteca Haskell: www.haskell.org/onlinereport/prelude-index.html