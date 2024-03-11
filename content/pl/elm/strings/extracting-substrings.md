---
date: 2024-01-20 17:45:37.337507-07:00
description: "Wyci\u0105ganie podci\u0105g\xF3w to tak naprawd\u0119 wycinanie kawa\u0142\
  k\xF3w tekstu z wi\u0119kszych \u0142a\u0144cuch\xF3w znak\xF3w. Programi\u015B\
  ci robi\u0105 to, \u017Ceby dosta\u0107 u\u017Cyteczne informacje,\u2026"
lastmod: '2024-03-11T00:14:08.488327-06:00'
model: gpt-4-1106-preview
summary: "Wyci\u0105ganie podci\u0105g\xF3w to tak naprawd\u0119 wycinanie kawa\u0142\
  k\xF3w tekstu z wi\u0119kszych \u0142a\u0144cuch\xF3w znak\xF3w. Programi\u015B\
  ci robi\u0105 to, \u017Ceby dosta\u0107 u\u017Cyteczne informacje,\u2026"
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
---

{{< edit_this_page >}}

## What & Why?
Wyciąganie podciągów to tak naprawdę wycinanie kawałków tekstu z większych łańcuchów znaków. Programiści robią to, żeby dostać użyteczne informacje, czyścić dane, czy też do zastosowań logiki biznesowej.

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
