---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja ciągów to technika, która pozwala programistom wstawiać wartości zmiennych bezpośrednio do ciągów znakowych. Umożliwia to dynamiczne generowanie ciągów, co jest szczególnie przydatne w przypadku wysyłania komunikatów na temat stanu aplikacji czy budowania skomplikowanych zapytań SQL.

## Jak to zrobić:

W latest Gleamie (v0.18.2, March 2022) interpolacja stringów nie jest dostępna natywnie. Ale jak można to obejść? Umożliwiają to funkcje `IO.int_to_string` i `IO.float_to_string` do konwersji typów numerycznych i operator `+` do łączenia stringów.

```Gleam
import gleam/io.{stdout, int_to_string}

fn main() {
  let name = "Kasia"
  let age = 26

  stdout("Cześć, moje imię to " + name + " i mam " + int_to_string(age) + " lat.\n")
}
```

Wywołanie tej funkcji wyświetli tekst:

```
Cześć, moje imię to Kasia i mam 26 lat.
```

## Deep Dive

Interpolacja ciągów pojawia się w wielu językach programowania. W dosłownym sensie, jest to wstawianie wartości zmiennych wewnątrz ciągów znakowych. Historia konceptu jest długa - zaczyna się od języków jak Perl i przekazuje do nowoczesnych języków jak Ruby czy Python.

Podobne funkcjonalności można osiągnąć poprzez formatowanie ciągów, takie jak `printf` w C lub `format` w Pythonie.

Jeśli chodzi o szczegóły implementacji, różne języki programowania korzystają z różnych strategii, od prostego zastępowania znacznika ciągu wartością zmiennej, po użycie specjalnych bibliotek do obsługi ciągów.

## Zobacz również:

- Dokumentacja oficjalna Gleam na temat ciągów: https://gleam.run/book/tour/strings.html
- Podobny koncept formatowania w Pythonie: https://docs.python.org/3/tutorial/inputoutput.html
- Inne podejście do formatowania w języku C: https://www.cplusplus.com/reference/cstdio/printf/