---
title:                "Gleam: Konwertowanie ciągu znaków na małe litery"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery jest jedną z podstawowych operacji w programowaniu. Jest to przydatne, ponieważ umożliwia nam ujednolicenie danych i zapewnienie spójności w naszym kodzie. W tym artykule dowiesz się, dlaczego warto dokonać takiej konwersji w języku programowania Gleam.

## Jak to zrobić

Aby przekonwertować ciąg znaków na małe litery w Gleam, możemy skorzystać z wbudowanej funkcji `String.to_lower`, która zwraca nowy ciąg znaków z przekonwertowanymi małymi literami. Poniżej znajduje się przykładowy kod Gleam, który to pokazuje:

```gleam
let tekst = "Witaj świecie!"
let nowy_tekst = String.to_lower(tekst)

// Output: "witaj świecie!"
```

W powyższym przykładzie widzimy, że po przekonwertowaniu pierwsza litera "W" została zamieniona na "w", a polskie znaki w słowie "świecie" zostały zachowane.

## Głębszy wgląd

Realizując operację konwersji na małe litery, należy pamiętać o tym, jakie dane mamy do dyspozycji i w jakim celu będą one wykorzystywane. W niektórych przypadkach może być konieczne wykonanie dodatkowych operacji, takich jak sprawdzenie długości tekstu czy usunięcie białych znaków przed przeprowadzeniem konwersji.

## Zobacz także

Jeśli chcesz poznać więcej o manipulowaniu tekstem w języku programowania Gleam, zapoznaj się z następującymi artykułami i dokumentacją:

- [Jak przekonwertować ciąg znaków na liczbę](https://przykladowaswitaczka.pl/konwersja_siertki_opglady.html)
- [Wszystko o pracy ze stringami w Gleam](https://gleam-lang.org/docs/strings)