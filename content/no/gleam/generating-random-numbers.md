---
title:                "Gleam: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig aspekt av dataanalyse og programmering. Det kan være nyttig for å lage pseudotilfeldige tall for testing og simuleringer, eller for å gi variasjon i et program.

## Hvordan

For å generere tilfeldige tall i Gleam kan vi bruke standardbiblioteket Random. Vi kan bruke følgende kode for å generere et tilfeldig tall mellom 1 og 10:

```Gleam
import Random

let random_int = Random.int(1, 10)

pub fn main() {
  IO.print("Tilfeldig tall: {random_int}")
}
```

Kjøring av denne koden vil gi følgende output:

```
Tilfeldig tall: 7
```

Vi kan også generere tilfeldige desimaltall ved å bruke Random.float-funksjonen. For eksempel kan vi generere et desimaltall mellom 0 og 1 med en nøyaktighet på to desimaler ved å bruke følgende kode:

```Gleam
import Random

let random_float = Random.float(0, 1, 2)

pub fn main() {
  IO.print("Tilfeldig desimaltall: {random_float}")
}
```

Output av denne koden kan for eksempel være:

```
Tilfeldig desimaltall: 0.78
```

## Deep Dive

For å forstå mer om hvordan tilfeldige tall blir generert i Gleam, kan vi se på implementasjonen av Random-modulen i standardbiblioteket. Random bruker en kryptografisk sikker verdigenerator for å produsere høyst tilfeldige tall. Denne verdigeneratoren blir initiert med et "seed", som er et tall som brukes som utgangspunkt for å generere andre tall. Seed-en kan bli satt manuelt, men blir som standard satt til å bruke systemtid. Dette sørger for at tallene som genereres er så tilfeldige som mulig.

## Se Også

- [Random-modulen i standardbiblioteket til Gleam](https://gleam.run/std/random.html)
- [Tilfeldige tall på Wikipedia (Engelsk)](https://en.wikipedia.org/wiki/Random_number_generation)