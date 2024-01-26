---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:45:49.134303-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Wyciąganie podłańcuchów to proces selekcji określonych fragmentów z łańcucha znaków. Programiści robią to, by analizować i manipulować danymi tekstowymi, wydobywając z nich użyteczne informacje.

## How to:
Gleam pozwala na łatwe operowanie na łańcuchach znaków. Oto przykład:

```gleam
import gleam/string

fn main() {
  let text = "Witaj, świecie"
  let world = string.slice(text, 7, 14)
  world
}
```

Wynik:
```
"świecie"
```

## Deep Dive
Przed pojawieniem się wysokopoziomowych języków takich jak Gleam, wyciąganie podłańcuchów było procesem żmudnym, często wymagającym pracy z wskaźnikami. Gleam, jako nowoczesny język funkcyjny, upraszcza te zadania. Alternatywami dla 'string.slice' mogą być funkcje jak 'string.split' czy wyrażenia regularne, choć każda z tych metod ma swoje idealne zastosowanie. Implementacja 'slice' w Gleamie jest optymalizowana pod kątem efektywności i bezpieczeństwa w użyciu.

## See Also
- [Elixir's String module](https://hexdocs.pm/elixir/String.html) - Gleam został stworzony dla wirtualnej maszyny Erlanga, więc dobrze jest porównać podejścia.
- [Rust's substring slicing](https://doc.rust-lang.org/std/primitive.str.html#method.get) - inny funkcjonalny język z bezpiecznym zarządzaniem pamięcią.
