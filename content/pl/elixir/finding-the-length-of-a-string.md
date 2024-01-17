---
title:                "Znalezienie długości ciągu znaków"
html_title:           "Elixir: Znalezienie długości ciągu znaków"
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości ciągu znaków jest kluczowym aspektem programowania. Pozwala nam to na mierzenie wielkości danych oraz wykonywanie różnych operacji, na przykład porównywanie lub wycinanie części tekstu. 

## Jak to zrobić:
W Elixirze, możemy użyć funkcji `String.length/1`, która przyjmuje jeden argument - ciąg znaków - i zwraca jego długość. Przykładowo, jeśli chcemy znaleźć długość słowa "Programowanie", wpisujemy:
```Elixir
String.length("Programowanie")
```
I otrzymujemy wynik: `13`. 

Możemy również wykorzystać funkcję `Enum.count/1`, która zlicza ilość elementów w danym ciągu znaków. W tym przypadku argument musi być listą, więc musimy sparsować nasz ciąg do listy, za pomocą funkcji `String.split/2`. Przykładowo:
```Elixir
String.split("Lorem ipsum dolor sit amet", "") |> Enum.count()
```
I otrzymujemy wynik: `26`.

## Głębszy wykład:
Długość ciągu znaków jest integralną częścią wielu języków programowania, w tym Elixira. Wcześniejsze wersje języka wykorzystywały funkcję `length/1`, jednak w wersji 1.7 została ona zastąpiona przez `String.length/1`, aby podkreślić, że jest specyficzna dla ciągów znaków. Alternatywnym sposobem zliczania długości jest użycie funkcji `byte_size/1`, która zwraca ilość bajtów w danym ciągu. 

## Zobacz również:
- Dokumentacja Elixira dla funkcji `String.length/1`: [https://hexdocs.pm/elixir/String.html#length/1](https://hexdocs.pm/elixir/String.html#length/1)
- Inne przydatne funkcje dla pracy z ciągami znaków w Elixirze: [https://blog.openfrost.pl/operacje-na-ciagach-znakow-w-elixir/](https://blog.openfrost.pl/operacje-na-ciagach-znakow-w-elixir/)