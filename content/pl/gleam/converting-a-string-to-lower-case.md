---
title:    "Gleam: Konwertowanie ciągu znaków na małe litery"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

"Zamiana na małe litery" jest popularną operacją w programowaniu, szczególnie w przypadku pracy z tekstem. Dzięki niej można łatwiej porównywać i sortować słowa, a także ułatwiać użytkownikom korzystanie z naszych aplikacji.

## Jak to zrobić

Aby zamienić łańcuch znaków na małe litery w Gleam, wystarczy użyć wbudowanej funkcji `String.to_lower`. Poniżej przedstawiamy prosty przykład, jak mogłoby to wyglądać w praktyce:

```Gleam
let tekst = "PRZYKŁADOWY TEKST"
let tekst_mniejszymi_literami = String.to_lower(tekst)
```

Jeśli nie chcesz nadpisywać wartości wcześniej zdefiniowanej zmiennej, możesz również użyć `String.to_lower_in_place`, aby dokonać zamiany w miejscu.

```Gleam
let inny_tekst = "INNY TEKST"
String.to_lower_in_place(inny_tekst)
```

Ten przykład zmieni wartość zmiennej `inny_tekst` na `inny tekst`.

## Głębszy zanurzenie

Warto zauważyć, że funkcja `String.to_lower` i `String.to_lower_in_place` są jedynie metodami skróconymi dla wywołania funkcji `String.map`, która przyjmuje dwa argumenty: funkcję przekształcającą i łańcuch znaków. Jest to przydatne, gdy chcemy wykonać bardziej złożone operacje zamiany na małe litery, np. zmieniać na małe tylko pierwszą literę w wyrazie.

```Gleam
let tekst = "PRZYKŁADOWY TEKST"
String.map(String.to_lower_case_first, tekst)
```

Możemy również użyć funkcji `String.map_indexed`, aby dokonać zmiany tylko na konkretnym indeksie łańcucha znaków.

```Gleam
let tekst = "PRZYKŁADOWY TEKST"
String.map_indexed(\i c -> if i == 0 { String.to_lower(c) } else { c }, tekst)
```

Dzięki temu możemy swobodnie dostosować funkcję przekształcającą do naszych indywidualnych potrzeb.

## Zobacz także

- Dokumentacja funkcji `String.to_lower` w języku polskim: [https://gleam.run/docs/stdlib/string#to_lower/1](https://gleam.run/docs/stdlib/string#to_lower/1)
- Poradnik na temat obsługi łańcuchów znaków w Gleam: [https://gleam.run/docs/guides/strings](https://gleam.run/docs/guides/strings)
- Przykłady użycia mapowania łańcuchów znaków w Gleam: [https://github.com/gleam-lang/gleam_stdlib/blob/master/string/tests/string_test.gleam#L163-L188](https://github.com/gleam-lang/gleam_stdlib/blob/master/string/tests/string_test.gleam#L163-L188)