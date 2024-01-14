---
title:    "Elm: Wycinanie podciągów"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

# Po co?

Czy kiedykolwiek zdarzyło ci się potrzebować tylko części ciągu znaków z tekstu? Może musisz wyodrębnić imię i nazwisko z pełnego adresu email, a może potrzebujesz tylko fragmentu adresu URL. W takich przypadkach bardzo przydatne jest umiejętne wyodrębnianie podciągów, czyli części ciągów znaków.

## Jak to zrobić?

W języku Elm do wyodrębniania podciągów służy funkcja `String.slice start end text`, gdzie `start` i `end` to numery indeksów, które definiują zakres podciągu do wyodrębnienia, a `text` to ciąg znaków, z którego chcemy wyodrębnić podciąg.

Przykładowo, jeśli chcemy wyodrębnić tylko pierwsze trzy litery z tekstu "Elm", możemy użyć kodu:

```
Elm ... "Elm" |> String.slice 0 3
```

Co zwróci nam podciąg "Elm".

## Głębszy zanurkowanie

Funkcja `String.slice` może nie tylko wyodrębniać fragmenty tekstu, ale również zwracać pozycję podciągu w tekście przy pomocy funkcji `String.indexes subStr text`. Możemy także wykorzystać funkcję `String.contains subStr text` do sprawdzenia, czy ciąg znaków zawiera dany podciąg.

Na przykład, chcąc wyodrębnić imię i nazwisko z adresu email "jan.kowalski@example.com", możemy użyć kodu:

```
"jan.kowalski@example.com" |> String.slice <| (String.indexes "@" <| String.toLower <| "jan.kowalski") + 1
```

Co zwróci nam podciąg "jan.kowalski".

# Zobacz również

- Dokumentacja Elm dla funkcji String: https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Przykładowy kod dla wyodrębniania podciągów w języku Elm: https://elm-lang.org/examples/substring
- Praktyczne zastosowanie funkcji `String.slice` na przykładzie analizy tekstu: https://ertuzio.com/extracting-text-from-a-string-in-elm/