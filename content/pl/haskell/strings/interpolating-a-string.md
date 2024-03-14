---
date: 2024-01-20 17:50:52.523618-07:00
description: "Interpolacja ci\u0105gu znak\xF3w polega na wstawianiu warto\u015Bci\
  \ zmiennych bezpo\u015Brednio w stringu. Programi\u015Bci robi\u0105 to, aby generowa\u0107\
  \ dynamiczne teksty z danych,\u2026"
lastmod: '2024-03-13T22:44:35.437397-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja ci\u0105gu znak\xF3w polega na wstawianiu warto\u015Bci zmiennych\
  \ bezpo\u015Brednio w stringu. Programi\u015Bci robi\u0105 to, aby generowa\u0107\
  \ dynamiczne teksty z danych,\u2026"
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolacja ciągu znaków polega na wstawianiu wartości zmiennych bezpośrednio w stringu. Programiści robią to, aby generować dynamiczne teksty z danych, łatwiej formatować wyjścia i ogólnie uprościć pisanie kodu.

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
