---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Ale co to i dlaczego?

Parsowanie daty z ciągu to przekształcanie tekstu reprezentującego datę względem określonego formatu w obiekt daty, umożliwiający łatwiejszą manipulację. Programiści to robią, aby uprościć przetwarzanie danych i interakcję użytkownika.

## Jak to zrobić:

W Elm stosowany jest następujący sposób na parsowanie daty z ciągu:

```elm
import Time
import Time.Extra

parseDate: String -> Maybe Time.Posix
parseDate dateString =
    Time.fromString dateString |> Maybe.andThen Time.Extra.dateTime

main =
    parseDate "2020-05-12T10:45:00Z"
        |> Maybe.map Time.toIsoString
        |> Maybe.withDefault "Niepoprawna data"
```

Output:

```elm
"2020-05-12T10:45:00Z"
```

## Głębsze spojrzenie

Elm jest młodym językiem, ale w jego bibliotece do zarządzania czasem można znaleźć metody do parsowania dat. Alternatywą dla tego jest używanie biblioteki third-party, takiej jak elm-date-extra, która oferuje wiele dodatkowych funkcji, ale może wprowadzać dodatkowe zależności.

Parsowanie daty z ciągu jest zwykle operacją niezawodną, ale w Elm jest ono typu `Maybe`, co oznacza, że może się nie udać i może zwrócić `Nothing`, jeśli format ciągu wejściowego jest niepoprawny. To jest zgodne z filozofią Elm, aby unikać błędów w czasie wykonywania.

## Zobacz też:

- Elm Time -> https://package.elm-lang.org/packages/elm/time/latest/
- Elm Date Extra -> https://package.elm-lang.org/packages/justinmimbs/date-extra/latest/