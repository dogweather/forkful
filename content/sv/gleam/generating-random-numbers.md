---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:12.365160-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga tal är en process där din kod skapar ett nummer som inte kan förutspås. Programmerare gör detta för allt från spellogik till säkerhetskryptering.

## How to:
```gleam
import gleam/int
import gleam/random

pub fn main() {
  let seed = random.default_seed()
  let (num, _new_seed) = random.int(seed, 1, 100)
  int.to_string(num)
}
```
Sample output: `42`

## Deep Dive
Randomisering i programmering har använts sedan datorns barndom för att simulera oförutsägbarhet och komplexa system. Historiskt sett har metoder varierat från enkla algoritmer som Linear Congruential Generator (LCG) till mer avancerade som Mersenne Twister. I Gleam används en pseudoslumptalsgenerator som ger en tillräckligt "slumpmässig" sekvens för de flesta användningsfall. Viktigt att notera är att pseudoslumptal inte är lämpliga för stark kryptografisk säkerhet; för dessa ändamål behövs hardware-baserade eller mer sofistikerade algoritmer.

Alternativ till Gleam's inbyggda random-modul inkluderar att binda till externa bibliotek skrivna i andra språk som tillhandahåller olika randomiseringstekniker.

Implementationen i Gleam använder sig av Erlang's :rand-modul under huven, vilket gör det robust och välpresterande, samtidigt som det håller Gleam's typsäkra snitt framåt.

## See Also
- Gleam's official `random` module documentation: https://hexdocs.pm/gleam_stdlib/gleam/random/
- Understanding randomness in computer programs: https://en.wikipedia.org/wiki/Random_number_generation
- More on pseudorandom number generators: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
