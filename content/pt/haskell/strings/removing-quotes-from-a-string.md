---
date: 2024-01-26 03:39:39.835016-07:00
description: "Como fazer: Em Haskell, podemos criar uma fun\xE7\xE3o que remove todas\
  \ as aspas de uma determinada string. \xC9 como dizer para as aspas vazarem, e garantir\
  \ que\u2026"
lastmod: '2024-03-13T22:44:46.613741-06:00'
model: gpt-4-0125-preview
summary: "Em Haskell, podemos criar uma fun\xE7\xE3o que remove todas as aspas de\
  \ uma determinada string."
title: Removendo aspas de uma string
weight: 9
---

## Como fazer:
Em Haskell, podemos criar uma função que remove todas as aspas de uma determinada string. É como dizer para as aspas vazarem, e garantir que elas captaram a mensagem.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell disse, \"Vamos aprender algumas funções!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Saída da amostra:

```
Haskell disse, Vamos aprender algumas funções!
```

## Mergulho Profundo
Era uma vez, antes das strings na programação serem tão comuns quanto vídeos de gatos na internet, lidar com texto era um negócio complicado. Mas à medida que as linguagens de programação evoluíram, strings se tornaram uma parte crucial da codificação. No entanto, as aspas permaneceram uma espada de dois gumes—essenciais para definir strings, mas uma praga quando incluídas como dados reais.

Alternativas? Em vez de espantar todas as aspas como se fossem moscas, você pode ser seletivo. Talvez você queira remover apenas as aspas mais externas (um clássico trim) ou lidar com aspas escapadas dentro de uma string.

Em termos de implementação, a função `removeQuotes` acima usa uma lambda para verificar cada caractere (`c`) para ver se é uma aspa incômoda e as filtra de acordo. Esta é uma abordagem direta, mas para textos maiores ou regras mais complexas, você pode querer olhar para bibliotecas de análise como `Parsec`, que podem oferecer mais requinte e poder no processamento de texto.

## Veja Também:
- Para amantes de regex: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Uma introdução suave às strings Haskell: [Aprenda Haskell Para o Grande Bem! - Começando](http://learnyouahaskell.com/starting-out#strings)
