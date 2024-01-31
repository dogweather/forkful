---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:45:37.337507-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/extracting-substrings.md"
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
