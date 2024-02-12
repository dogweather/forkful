---
title:                "Capitalizando uma string"
aliases:
- /pt/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:17.825846-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizando uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O quê & Por quê?
Capitalizar uma string envolve transformar a primeira letra de uma string dada em maiúscula, enquanto garante que as restantes letras permaneçam em minúscula. Programadores fazem isso para formatar saídas, aderir à correção gramatical em textos ou melhorar a legibilidade de dados gerados.

## Como fazer:
Em Haskell, você pode capitalizar uma string usando a biblioteca padrão sem a necessidade de quaisquer bibliotecas de terceiros.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Uso de exemplo:
main = putStrLn $ capitalize "hello world"
```

Saída:
```
Hello world
```

Para cenários mais complexos ou facilidade de uso, você pode querer usar uma biblioteca de terceiros, como `text`, que é popular para manipulação eficiente de strings em Haskell.

Primeiro, você precisa adicionar `text` às dependências do seu projeto. Então, você pode usar suas funções para capitalizar uma string da seguinte forma:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Uso de exemplo com a biblioteca text:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Saída:
```
Hello world
```

Ambos estes exemplos demonstram formas simples, porém efetivas, de capitalizar uma string em Haskell, com ou sem bibliotecas de terceiros.
