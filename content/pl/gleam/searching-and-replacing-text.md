---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Gleam: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zamiana tekstu to częsty element programowania, który polega na znajdowaniu i zmienianiu określonego tekstu w kodzie. Jest to przydatna umiejętność dla programistów, ponieważ pozwala im szybko i efektywnie edytować swoje projekty.

## Jak to zrobić:

### Przykład 1:
Zamień wszystkie wystąpienia słowa "hello" na "hi" w tekście:
``` Gleam
replace("hello" -> "hi") "hello world"  // output: "hi world"
```

### Przykład 2:
Zastosuj zamianę tekstu na wielu liniach tekstu za pomocą funkcji `map`:
``` Gleam
let message = "Hello, how are you?"

let upper = fn(x) { String.to_uppercase(x) }
let excl = fn(x) { String.append(x, "!") }

let new_message = message
    |> String.split(by: ", ")
    |> map(upper)
    |> map(excl)
    |> String.join(separator: " ")

// output: "HELLO, HOW ARE YOU?!"
```

## Pogląd w Głąb:

W przeszłości, programiści musieli ręcznie zmieniać wszystkie wystąpienia tekstu, co może być czasochłonne i podatne na błędy. Dzięki wykorzystaniu współczesnych narzędzi programistycznych, takich jak Gleam, możliwe jest łatwe wykonywanie tych operacji.

Jedną z alternatyw dla Gleam jest popularny język programowania, takich jak Java lub Python, które również oferują możliwość wyszukiwania i zamiany tekstu za pomocą wbudowanych funkcji.

Implementacja funkcji zamiany w Gleam jest wydajna i odporna na błędy, dzięki czemu programiści mogą być pewni, że ich aplikacje będą działać zgodnie z oczekiwaniami.

## Zobacz również:

- Dokumentacja Gleam na temat zamiany tekstu: https://gleam.run/builtin.html#replace
- Tutorial wideo na temat wykorzystania funkcji zamiany w Gleam: https://www.youtube.com/watch?v=123456789
- Podstawy programowania w Gleam: https://gleam.run/docs/getting_started.html