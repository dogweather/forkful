---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-01-20T17:52:38.000325-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Debugowanie to proces szukania i usuwania błędów z kodu. Wypisywanie informacji diagnostycznych pomaga programistom monitorować i rozumieć, co się dzieje w trakcie działania programu.

## Jak to zrobić?

```gleam
import gleam/io

pub fn main() {
  let value = "Debugging in Gleam!"
  io.debug(value) // Prints to the console: "Debugging in Gleam!"
}
```

Wynikowy output:
```
Debugging in Gleam!
```

Użyj funkcji `io.debug` aby wypisać zmienne lub wiadomości podczas debugowania. Pamiętaj o ich usunięciu, gdy twój kod będzie gotowy do produkcji.

## Wgłębiamy się

Wypisywanie do konsoli to stara, ale skuteczna technika debugowania. Powstała wraz z pierwszymi komputerami i wciąż jest podstawowym narzędziem. Alternatywą dla print-debuggingu są debuggery, które oferują zaawansowane funkcje jak breakpointy czy analiza zmienności. Gleam naśladuje podejście Erlianga i Elixira, czerpiąc z ich ekosystemu i narzędzi do debugowania, pozwalając na lepsze zarządzanie dużymi systemami.

## Zobacz również

- [Erlang's guide to debugging](http://erlang.org/doc/apps/debugger/debugger_chapter.html)
- [Elixir's `IO.inspect` for comparison](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Effective debugging strategies](https://betterprogramming.pub/the-effective-guide-to-debugging-396451fbf36e?gi=624a0742dbd1)
