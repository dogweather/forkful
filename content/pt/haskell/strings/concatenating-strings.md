---
date: 2024-01-20 17:34:55.212749-07:00
description: "Concatenar strings \xE9 juntar duas ou mais sequ\xEAncias de caracteres\
  \ para formar uma nova string. Programadores fazem isso para manipular texto, construir\u2026"
lastmod: '2024-02-25T18:49:44.238871-07:00'
model: gpt-4-1106-preview
summary: "Concatenar strings \xE9 juntar duas ou mais sequ\xEAncias de caracteres\
  \ para formar uma nova string. Programadores fazem isso para manipular texto, construir\u2026"
title: Concatenando strings
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Concatenar strings é juntar duas ou mais sequências de caracteres para formar uma nova string. Programadores fazem isso para manipular texto, construir mensagens ou gerar saída de dados de maneira dinâmica.

## Como Fazer:

Concatenar strings em Haskell é simples. Você pode usar o operador `++` ou a função `concat`. Aqui estão alguns exemplos:

```haskell
main :: IO ()
main = do
  let hello = "Olá"
  let world = "Mundo"
  
  -- Usando o operador ++
  putStrLn (hello ++ " " ++ world) -- "Olá Mundo"
  
  -- Usando concat
  putStrLn (concat [hello, " ", world]) -- "Olá Mundo"
```

## Mergulho Profundo:

Historicamente, a concatenação de strings é uma operação básica na maioria das linguagens de programação, e Haskell não é exceção. Alternativas para concatenação em Haskell incluem o uso de `StringBuilder` em ambientes onde a performance é crítica ou até mesmo o uso de `intercalate` de `Data.List` se você estiver lidando com uma lista de strings.

A implementação da concatenação em Haskell é otimizada para ser tão eficiente quanto possível, mas é importante lembrar que o operador `++` tem complexidade O(n) em relação ao comprimento da primeira lista, então use-o sabiamente.

## Veja Também:

Para mais detalhes e exemplos sobre manipulação de strings em Haskell, confira os seguintes links:

- [Haskell Wiki sobre strings](https://wiki.haskell.org/Strings)
- [Hackage: pacote `text`](https://hackage.haskell.org/package/text)
- [LYAH: Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
