---
title:                "Gleam: Wyszukiwanie i zastępowanie tekstu."
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem w swoim kodzie musimy dokonać zmian, np. poprawić literówki, zmienić nazwy zmiennych lub wyrażeń, lub po prostu zmienić sposób wyświetlania tekstu. Aby oszczędzić sobie czasu i wysiłku, warto poznać możliwości programowania w Gleam, które ułatwią nam przeszukiwanie i zamienianie tekstu.

## Jak to zrobić

W Gleam istnieją dwa główne sposoby na przeszukiwanie i zamienianie tekstu: wyrażenia regularne i funkcja `String.replace()`. Wyrażenia regularne są potężnym narzędziem, umożliwiającym precyzyjne dopasowanie wzorców w tekście. Używają one specjalnych znaków, takich jak `*` czy `+`, które sygnalizują np. powtórzenia lub opcjonalne fragmenty tekstu. Natomiast funkcja `String.replace()` pozwala nam dokonać prostej zamiany tekstu na określony inny tekst.

Przykład z wykorzystaniem wyrażeń regularnych:

```Gleam
let pattern = "~[aeiou]~"
let input = "Lorem ipsum dolor sit amet"
let replaced = Regex.replace(pattern, input, "X")
```

Przykład z użyciem funkcji `String.replace()`:

```Gleam
let input = "Hello, world!"
let replaced = String.replace(input, "Hello", "Hi")
```

W obu przypadkach, warto zwrócić uwagę na odpowiednią składnię i wykorzystanie odpowiednich bibliotek w zależności od tego, czy chcemy używać wyrażeń regularnych czy funkcji `String.replace()`.

## Zanurzenie w temat

Wyrażenia regularne mogą być czasem trudne do zrozumienia, zwłaszcza dla początkujących programistów. Warto zapoznać się z podstawowymi zasadami ich tworzenia i zastosowania, aby móc wykorzystać je w swoim kodzie w sposób efektywny. Natomiast funkcja `String.replace()` może być przydatna w szybkich i prostych zamianach tekstu, ale również warto zapoznać się z jej parametrami i dostępnymi opcjami, aby dokonać bardziej skomplikowanych zmian.

## Zobacz również

- Dokumentacja Gleam na temat wyrażeń regularnych: [link](https://gleam.run/modules/regex/latest/)
- Dokumentacja Gleam na temat funkcji `String.replace()`: [link](https://gleam.run/modules/string.html#replace)
- Przykłady użycia wyrażeń regularnych w Gleam: [link](https://medium.com/@adamrenklint/regex-in-gleam-c6eacfd39b88)
- Przykłady użycia funkcji `String.replace()` w Gleam: [link](https://github.com/gleam-lang/gleam/blob/master/examples/string_replace_source.gleam)