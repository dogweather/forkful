---
title:    "Gleam: Konkatenacja ciągów znaków"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

Gleam: Jak łączyć ciągi znaków

## Dlaczego

Konkatenacja (łączenie) ciągów znaków to powszechnie stosowana technika w programowaniu, szczególnie w przypadku języków takich jak JavaScript czy Python. Pozwala ona na łączenie kilku fragmentów tekstu w jeden dłuższy ciąg, co jest niezbędne w wielu aplikacjach. W języku Gleam możemy również skorzystać z tej funkcjonalności, co ułatwia nam pracę i tworzenie bardziej rozbudowanych programów.

## Jak to zrobić

Aby skorzystać z funkcji łączenia ciągów znaków w Gleam, musimy użyć operatora `++`. Poniżej znajdują się przykładowe kody oraz wyniki dla dwóch ciągów znaków: "Hello" i "world".

```Gleam
let string1 = "Hello"
let string2 = "world"
let result = string1 ++ " " ++ string2
```
Wynik: "Hello world"

Możemy również łączyć więcej niż dwa ciągi znaków:

```Gleam
let string1 = "I love"
let string2 = "programming"
let string3 = "in Gleam!"
let result = string1 ++ " " ++ string2 ++ " " ++ string3
```
Wynik: "I love programming in Gleam!"

## Głębsza analiza

W języku Gleam, konkatenacja ciągów znaków jest realizowana przez funkcję `String.concat`, która jako argument przyjmuje listę ciągów znaków i zwraca jeden łączony ciąg. Inną przydatną funkcją jest `String.join`, która pozwala na połączenie listy ciągów znaków, oddzielając je podanym separatorem.

Warto także wspomnieć, że w Gleamie ciągi znaków są stałe, więc nie można zmieniać już utworzonego ciągu. Jeśli chcemy dokonać zmiany w ciągu znaków, musimy utworzyć nowy ciąg.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o manipulowaniu ciągami znaków w języku Gleam, możesz zapoznać się z oficjalną dokumentacją:
- [String](https://gleam.run/core/string/)
- [List](https://gleam.run/core/list/)
- [Join](https://gleam.run/core/string/#join)
- [Concat](https://gleam.run/core/string/#concat)