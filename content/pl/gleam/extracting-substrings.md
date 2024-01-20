---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Ekstrakcja podciągów to proces wydobywania mniejszych nitek znaków, z większych ciągów tekstowych. Programiści często korzystają z niej, aby przekształcić, filtrować lub analizować dane.

## Jak to zrobić:

Rozważmy przykład w języku Gleam:
```Gleam 
  let tekst = "Cześć, Świecie"
  let podciąg = string.slice(tekst, 7, 13)
  // Wynikiem będzie "Świecie"
```
Tutaj, za pomocą funkcji `string.slice()`, pobieramy podciąg z `tekst` zaczynając od 7. pozycji i kończąc na 13.

## Wgląd Głębszy:

Możliwość ekstrakcji podciągów pojawiła się w większości języków programowania historycznie rano, ułatwiając manipulację danymi tekstowymi. Alternatywą dla `string.slice()` w Gleam jest `string.split()`, ale ta funkcja dzieli tekst na względem określonego separatora, a nie indeksów.

Szczegóły implementacji Gleam są jasne i proste. Faktycznie `string.slice()` wywołuje tylko natywną funkcję `slice()` Erlanga na tyłach.

## Zobacz także:

Polecamy zapoznanie się z oficjalnymi źródłami dokumentacji:
- Gleam String API docs: [https://hexdocs.pm/gleam_stdlib/gleam/string.html](https://hexdocs.pm/gleam_stdlib/gleam/string.html)
- Erlang string module: [https://erlang.org/doc/man/string.html](https://erlang.org/doc/man/string.html)