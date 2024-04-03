---
date: 2024-01-20 17:50:52.523618-07:00
description: "Jak to zrobi\u0107: W Haskellu, interpolacja nie jest wbudowana jak\
  \ w innych j\u0119zykach. Musimy u\u017Cy\u0107 biblioteki, takiej jak `text` i\
  \ jej quasi-quoter\xF3w. Oto jak."
lastmod: '2024-03-13T22:44:35.437397-06:00'
model: gpt-4-1106-preview
summary: "W Haskellu, interpolacja nie jest wbudowana jak w innych j\u0119zykach."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

## Jak to zrobić:
W Haskellu, interpolacja nie jest wbudowana jak w innych językach. Musimy użyć biblioteki, takiej jak `text` i jej quasi-quoterów. Oto jak:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Data.Text
import Data.Text.IO as T
import Text.Shakespeare.Text (lt)

name = "świat"
age = 30

main = T.putStrLn [lt|Cześć, #{name}! Masz już #{age} lat.|]
```

Po uruchomieniu otrzymasz:

```
Cześć, świat! Masz już 30 lat.
```

## Głębsze spojrzenie
Haskell początkowo nie miał wbudowanej interpolacji ciągów znaków. Musieliśmy używać konkatenacji i funkcji `show`. Doprowadziło to do powstania różnych bibliotek, które to upraszczały, jak `text` i `interpolate`.

Alternatywą dla Shakespeare'a jest `fmt`, który używa type-safe formatowania, również wart uwagi. Jego użycie wygląda tak:

```Haskell
import Fmt

main = putStrLn (format "Cześć, {}! Masz już {} lat." name age)
```

Co do implementacji, `text` używa quasi-quoterów (`[lt|...|]`) do analizowania tekstów i zastępowania wyrażeń zawartych w `#{}`. `fmt` korzysta z type-safe DSL, który sprawdzi poprawność typów podczas kompilacji.

## Zobacz także
- `text` na Hackage: https://hackage.haskell.org/package/text
- `fmt` na Hackage: https://hackage.haskell.org/package/fmt
- Tutorial interpolacji w Haskellu: https://wiki.haskell.org/Quasiquotation
- Dokumentacja TemplateHaskell: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/template-haskell.html
