---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Gleam: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy przy pisaniu kodu musimy zmienić pewne fragmenty tekstu w wielu miejscach. Może być to nazwa zmiennej, ścieżka do pliku, czy też cały blok kodu. W takich przypadkach bardzo pomocne jest narzędzie do wyszukiwania i zamieniania tekstu, które pozwala na szybką i precyzyjną modyfikację naszego kodu.

## Jak to zrobić?

Aby przeprowadzić operację wyszukiwania i zamieniania tekstu w Gleam, użyjemy funkcji `String.replace()`. Przykładowe użycie wyglądać może następująco:

```Gleam
let tekst = "Witaj Świecie!"
let zamieniony_tekst = String.replace(tekst, "Świecie", "Gleam")
```

W wyniku otrzymamy tekst "Witaj Gleam!". Warto zauważyć, że funkcja ta zwraca nowy tekst, a nie modyfikuje oryginalnego.

Możemy również wykorzystać wyrażenia regularne, aby precyzyjniej określić czego szukamy. Na przykład, jeśli chcemy zmienić wszystkie litery "a" na wielkie, możemy użyć wyrażenia `[a]`:

```Gleam
let tekst = "ala ma kota"
let zamieniony_tekst = String.replace(tekst, /[a]/, "A")
```

Wynik to "Ala mA kotA". Oczywiście, istnieje wiele innych zastosowań funkcji `replace()` i warto zapoznać się z jej dokumentacją, aby wykorzystać ją w pełni.

## Deep Dive

Funkcja `replace()` ma jeszcze jedną opcję, która pozwala na podanie maksymalnej liczby zamian. Domyślnie jest ona ustawiona na `None`, co oznacza, że wszystkie wystąpienia zostaną zamienione. Jednak jeśli wpiszemy konkretną liczbę, np. `Some(2)`, tylko pierwsze dwa wystąpienia zostaną zamienione.

Ponadto, możemy przekazać również funkcję do wykonania po każdej zmianie. Jest to przydatne np. gdy chcemy dodać prefix lub suffix do każdego wyniku zamiany.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o wykorzystaniu funkcji `replace()` w Gleam, możesz zajrzeć do dokumentacji języka lub przeczytać artykuł na temat wyrażeń regularnych: 

- [Dokumentacja funkcji `String.replace()`](https://gleam.run/manual/stdlib.html#string-module)
- [Wyrażenia regularne w Gleam](https://itnext.io/regular-expressions-in-gleam-de6dd3dcd739)