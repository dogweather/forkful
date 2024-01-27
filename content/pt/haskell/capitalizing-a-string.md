---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Capitalizar uma string significa transformar todas as letras de uma palavra ou sentença em maiúsculas. Programadores fazem isso para padronizar dados, como nomes próprios em formulários, e para destacar certos elementos textuais em interfaces de usuário.

## Como Fazer:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper

-- Exemplo de uso:
main :: IO ()
main = putStrLn (capitalize "olá, Haskell!")

-- Saída:
-- OLÁ, HASKELL!
```

## Mergulho Profundo

Capitalizar strings não é complicado, mas é um ponto de entrada classicão para se familiarizar com funções em Haskell. Historicamente, o tratamento de strings em programação muitas vezes inclui capitalização, que pode ser feito de várias maneiras. Por exemplo, em Haskell, você pode criar uma função personalizada como `capitalize` ou usar funções prontas de bibliotecas. No nosso caso, usamos `toUpper` do módulo `Data.Char`. Detalhe importante: esta função lida bem com caracteres ASCII, mas se você precisa de suporte a Unicode, considere o módulo `Data.Text`, que é mais robusto para manipulação de textos.

Alternativas incluem o uso da biblioteca `Data.Text` para lidar com `Text` em vez de `String`, o que é mais eficiente em termos de desempenho e memória. 

Exemplo com `Data.Text`:

```Haskell
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toUpper)

capitalizeText :: Text -> Text
capitalizeText = T.map toUpper

-- Exemplo de uso com Data.Text:
main :: IO ()
main = putStrLn . T.unpack $ capitalizeText (T.pack "olá, mundo com Text!")

-- Saída:
-- OLÁ, MUNDO COM TEXT!
```

## Veja Também

- Documentação do `Data.Char`: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html
- Documentação do `Data.Text`: https://hackage.haskell.org/package/text-2.0/docs/Data-Text.html
- Haskell Wiki sobre `String`: https://wiki.haskell.org/String
- Artigo sobre performance de `String` vs `Text`: https://wiki.haskell.org/Performance/String
