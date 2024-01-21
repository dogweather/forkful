---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:50:53.689231-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Interpolacja napisów pozwala wstawiać zmienne lub wyrażenia bezpośrednio do ciągów tekstowych. Programiści używają jej dla uproszczenia łączenia napisów i wartości, co czyni kod bardziej czytelnym.

## How to: (Jak to zrobić:)
```gleam
fn main() {
  let name = "świecie"
  let greeting = "Cześć, \(name)!"
  io.println(greeting)
}

// Output: Cześć, świecie!
```

## Deep Dive (W głąb tematu)
Historia interpolacji napisów sięga języków jak Perl czy Ruby, gdzie znacznie ułatwiła formatowanie napisów. Alternatywą dla interpolacji jest konkatenacja przy użyciu operatorów, ale jest ona mniej przejrzysta i może prowadzić do błędów. W Gleam, proces ten jest zaimplementowany tak, aby interpolowane wartości były automatycznie konwertowane na napisy, upraszczając pracę programisty.

## See Also (Zobacz również)
- More on Rust-inspired syntax (used in Gleam): [https://rust-lang.org](https://www.rust-lang.org/)
- Community discussions on string interpolation: [Gleam forum](https://github.com/gleam-lang/gleam/discussions)

Proszę zwrócić uwagę, że linki do dodatkowych źródeł mogą prowadzić do stron po angielsku, jako że Gleam jest stosunkowo nowym językiem i jego społeczność dopiero się rozwija.