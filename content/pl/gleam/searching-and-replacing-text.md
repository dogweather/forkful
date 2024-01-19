---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Poszukiwanie i zastępowanie tekstu to jedna z podstawowych operacji w programowaniu; pozwala na manipulowanie danymi tekstowymi. Programiści robią to, aby modyfikować, poprawiać lub personalizować dane na podstawie określonych warunków.

## Jak to zrobić:

Świetny sposób na przeszukiwanie i zamianę tekstu w Gleam to użycie funkcji `replace`. Zobaczmy, jak to działa:

```Gleam
import gleam/string

let tekst = "Programowanie to dobra zabawa"
let nowy_tekst = string.replace("zabawa", "przyjemność", tekst)
```

Output: 

```Gleam
"Programowanie to dobra przyjemność"
```

Zaczynamy od zaimportowania modułu `gleam/string`, a następnie używamy funkcji `replace`, aby zamienić słowo "zabawa" na "przyjemność".  

## Zagłębianie się

Historia funkcji wyszukiwania i zastępowania sięga początków informatyki. Wykorzystywana jest nie tylko w programowaniu, ale także w przetwarzaniu tekstu i innych zadaniach związanych z danymi.

Jest wiele innych technik wyszukiwania i zastępowania tekstu, które różnią się w zależności od języka programowania. W Pythonie na przykład można użyć metody `.replace()`, a w Java funkcji `replace()`.

Co do implementacji, Gleam korzysta z implementacji Elixira, który z kolei korzysta z modułu Erlang :binary, zapewniając efektywne i niezawodne operacje na ciągach.

## A także  

Jeśli chcesz dalej zgłębiać temat przeszukiwania i zastępowania tekstu, zapraszamy do odwiedzenia następujących zasobów:  

[Algorytm KMP (Knuth–Morris–Pratt Algorithm)](https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm) 
[Algorytm Boyera-Moore’a (Boyer–Moore string-search algorithm)](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm)
[Dokumentacja modułu String w Gleam](https://hexdocs.pm/gleam_stdlib/gleam/string/)