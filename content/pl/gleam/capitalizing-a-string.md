---
title:                "Gleam: Zapisywanie ciągu znaków wielką literą"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy w trakcie pisania kodu musimy dokonać zmiany na łańcuchu znaków, takiej jak zmiana formatu na wszystkie wielkie litery lub zmiana pierwszej litery na wielką. Aby ułatwić to zadanie, przydatną funkcją w języku Gleam jest capitalize, która pozwala na proste i szybkie kapitalizowanie łańcuchów znaków.

## Jak to zrobić

Aby skorzystać z funkcji capitalize, musimy najpierw zaimportować moduł string i skorzystać z dostępnego w nim algorytmu. Poniżej przedstawiono przykładowy kod w języku Gleam w celu kapitalizacji nazwiska "kowalski":

```
Gleam
import string

let nazwisko = string.capitalize("kowalski")

io.println(nazwisko)

// Output: Kowalski
```

Jak widać, funkcja capitalize działa na podobnej zasadzie jak funkcja upper w innych językach programowania. Możemy również wykorzystać tę funkcję do kapitalizacji całego łańcucha znaków:

```
Gleam
import string

let tekst = "witaj w świecie programowania"
let kapitalizowany_tekst = string.capitalize(tekst)

io.println(kapitalizowany_tekst)

// Output: Witaj w świecie programowania
```

## Głębsze zanurzenie

W przypadku funkcji capitalize warto zauważyć, że jest ona zastosowana jako część modułu string, który zawiera również inne przydatne metody do manipulacji łańcuchami znaków. Ta konkretna metoda przeprowadza kapitalizację wszystkich liter poza pierwszą, co może być przydatne w niektórych przypadkach. Możemy również wykorzystać funkcję capitalize do stworzenia własnego algorytmu kapitalizacji dopasowanego do naszych potrzeb.

## Zobacz też

- Dokumentacja modułu string w języku Gleam: https://gleam.run/modules/string.html
- Przykładowe wykorzystanie capitalize w języku Gleam: https://github.com/gleam-lang/gleam_stdlib/blob/master/std/string/string.gleam#L11-L25