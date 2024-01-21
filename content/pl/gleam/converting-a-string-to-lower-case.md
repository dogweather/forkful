---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:38:18.684874-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
"Co i Dlaczego?"

Zamiana łańcucha znaków na małe litery umożliwia standaryzację danych tekstowych. Programiści robią to, aby ułatwić porównywanie i przetwarzanie tekstu.

## How to:
"Jak to zrobić:"

Gleam ma wbudowaną funkcję do manipulacji ciągami znaków. Oto jak wygląda w akcji:

```gleam
import gleam/string

fn to_lower_case_example() {
  let my_string = "Witaj, Świecie!"
  string.lowercase(my_string)
}

fn main() {
  to_lower_case_example() |> io.debug
}
```

Przykładowe wyjście:
```
"witaj, świecie!"
```

## Deep Dive
"Dokładniejsze spojrzenie":

W przeszłości, zamiana na małe litery była różnie implementowana w różnych językach programowania, często uwzględniając ustawienia regionalne. Gleam używa podejścia Unicode do konwersji, co zapewnia konsystencję między różnymi systemami. Alternatywnie, można użyć funkcji specyficznych dla poszczególnych lokalizacji, ale w Gleam priorytetem jest klarowność i prostota. Wewnętrznie, `string.lowercase` przekształca każdy znak w stringu zgodnie z zasadami Unicode.

## See Also
"Zobacz również":

- Unicode Case Mapping Info: [https://www.unicode.org/reports/tr21/tr21-5.html](https://www.unicode.org/reports/tr21/tr21-5.html)