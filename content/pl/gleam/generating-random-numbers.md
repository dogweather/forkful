---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:48:57.782346-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Generowanie liczb losowych to proces tworzenia sekwencji liczb, które są w jakiś sposób nieprzewidywalne. Programiści wykorzystują je do wielu celów, takich jak testowanie i symulacje, gry oraz zabezpieczenia.

## How to: (Jak to zrobić:)
```gleam
import gleam/random
import gleam/int

fn main() {
  // Seed the random generator - replace with a suitable seed source
  let seed = random.seed(12345)
  let (num, next_seed) = random.int(seed, 1, 100)
  
  int.to_string(num) // Użyj tej linii, aby wydrukować liczbę jako ciąg znaków
}
```

Sample output (Przykładowe wyniki):
```
"47"
```

## Deep Dive (Dogłębna analiza)
Generowanie liczb losowych ma długą historię w informatyce. Początkowo wykorzystywano deterministyczne algorytmy; tak naprawdę są one "pseudo-random", bo wynikają z określonej sekwencji. Istnieją rożne metody, np. linear congruential generators (LCG) czy Mersenne Twister. W Gleam, używa się generatorów zaimplementowanych w języku Erlang, które są częścią BEAM VM, zapewniających solidne źródło losowości.

## See Also (Zobacz także)
- Gleam's Random module documentation: https://hexdocs.pm/gleam_stdlib/gleam/random/
- Wikipedia on Pseudo-random number generators: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- Erlang's :rand module documentation for more insights (used under the hood): http://erlang.org/doc/man/rand.html