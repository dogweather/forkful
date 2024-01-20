---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
W informatyce, zamienianie znaków w łańcuchu na wielkie litery to znany zabieg. Programiści używają go głównie dla ujednolicenia danych i poprawienia czytelności tekstów używanych w interfejsie użytkownika.

## How to: (Jak to zrobić:)
```gleam
import gleam/string

pub fn capitalize(text: String) -> String {
  string.capitalize(text)
}

fn main() {
  let example = "witaj, świecie!"
  let capitalized_example = capitalize(example)
  io.println(capitalized_example)
}
```
Wynik działania:
```
Witaj, świecie!
```

## Deep Dive (Dogłębna analiza)
Funkcja zmiany tekstu na wielkie litery ma swoje korzenie w maszynach do pisania i wczesnych komputerach, gdzie niektóre modele miały tylko wielkie litery. Stosowanie wielkich liter ułatwia wyróżnienie ważnej informacji lub nagłówków. Alternatywą może być użycie funkcji `toUpperCase`, która zmienia wszystkie znaki na wielkie, co jednak może nie być pożądane w niektórych kontekstach. W języku Gleam, używamy funkcji `capitalize` z modułu `string`, która zmienia pierwszą literę na wielką, resztę pozostawiając bez zmian. Ważne jest, aby pamiętać, że zachowanie tej funkcji może być zależne od lokalizacji i specyfiki języka, szczególnie w kontekście znaków diakrytycznych.