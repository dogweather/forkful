---
date: 2024-01-20 17:45:37.337507-07:00
description: "How to: Elm, cho\u0107 mo\u017Ce nie jest a\u017C tak popularny jak\
  \ JavaScript czy Python, daje prosty i wydajny spos\xF3b na prac\u0119 z ci\u0105\
  gami znak\xF3w, a wyci\u0105ganie podci\u0105g\xF3w\u2026"
lastmod: '2024-04-05T21:53:36.747971-06:00'
model: gpt-4-1106-preview
summary: "Elm, cho\u0107 mo\u017Ce nie jest a\u017C tak popularny jak JavaScript czy\
  \ Python, daje prosty i wydajny spos\xF3b na prac\u0119 z ci\u0105gami znak\xF3\
  w, a wyci\u0105ganie podci\u0105g\xF3w jest jednym z podstawowych narz\u0119dzi."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## How to:
```Elm
import String exposing (slice)

-- Przykład użycia `slice` do wyjęcia części stringa:
substringExample : String
substringExample =
    let
        text = "Witaj, świat programowania w Elm!"
        startIndex = 7
        endIndex = 13
    in
    slice startIndex endIndex text  -- "świat"

-- Wynik:
-- "świat"
```

## Deep Dive
Elm, choć może nie jest aż tak popularny jak JavaScript czy Python, daje prosty i wydajny sposób na pracę z ciągami znaków, a wyciąganie podciągów jest jednym z podstawowych narzędzi. W Elm korzystamy z funkcji `slice`, która działa podobnie do tej znanej z JavaScript – określamy indeks początkowy i końcowy, by wyciąć pożądany fragment tekstu.

Alternatywą jest użycie funkcji `String.left` i `String.right`, które pozwalają na podcięcie ciągu znaków od lewej lub prawej strony. Co do implementacji, Elm robi to bezpieczniej niż wiele języków, ponieważ ma stały system, który pomaga w uniknięciu błędów typu out-of-bound access (próba dostępu poza zakresem).

Historia Elm pokazuje, że język od początku był projektowany z myślą o bezpieczeństwie i łatwości użycia, co ma odzwierciedlenie także w operacjach na stringach.

## See Also
- Dokumentacja Elm na temat pracy ze stringami: [String](https://package.elm-lang.org/packages/elm/core/latest/String)
- Inny przydatny pakiet do zaawansowanych operacji na stringach: [elm-string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
