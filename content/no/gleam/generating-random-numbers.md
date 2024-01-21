---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:17.159108-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall betyr å lage tall som ikke kan forutsies – de er akkurat som de høres ut: tilfeldige. Programmerere gjør dette for spill, testing eller sikkerhet – essensielt hvor uberegnelighet er nødvendig.

## Hvordan:
Gleam gjør dette enkelt med standard bibliotekfunksjoner. Her er noen eksempler:

```gleam
import gleam/random

// Få et tilfeldig heltall
let tall = random.int(1, 10)
// Ut: En tilfeldig verdi mellom 1 og 10

// Få en tilfeldig streng
let streng = random.string(10)
// Ut: En tilfeldig streng på 10 tegn
```

## Dypdykk:
Tilfeldige tall i programmering har vært viktige siden datamaskinenes barndom, for alt fra simuleringer til kryptografi. Alternativer til Gleams `random`-modul kan være å bruke eksterne biblioteker, eller for mer vitenskapelig arbeid, spesialiserte algoritmer som Mersenne Twister. I Gleam, genereres tilfeldige tall ved å bruke "seeded" pseudotilfeldige tallgeneratorer, noe som betyr at tallene er i praksis forutsigbare hvis vi kjenner "seed"-verdien.

## Se Også:
- Wikipedia-artikkel om pseudotilfeldige tallgeneratorer: [wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- En diskusjon om tilfeldighet i programmering på Stack Overflow: [stackoverflow.com/questions/](https://stackoverflow.com/questions/tagged/random)