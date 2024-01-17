---
title:                "Generering av tilfeldige tall"
html_title:           "Rust: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Generering av tilfeldige tall er en viktig del av programmering, da det tillater oss å lage ulike, unike verdier på en enkel måte. Dette er spesielt nyttig i spill, simuleringer og kryptografi, der tilfeldige tall er nødvendige for å skape et variert og sikkert miljø.

## Hvordan:

Å generere tilfeldige tall i Rust er enkelt og innebærer bruk av standardbibliotekets funksjoner. Ved å importere "rand" biblioteket og bruke "thread_rng" funksjonen, kan vi opprette en generator som vil produsere et tilfeldig tall hver gang den kalles.

```Rust
use rand::Rng;
let mut rng = rand::thread_rng();
let random_number: u8 = rng.gen();
```

I dette eksempelet bruker vi "gen" funksjonen for å generere et tilfeldig tall mellom 0 og 255 og lagrer det i variabelen "random_number". Vi kan også bruke "gen_range" funksjonen for å lage tilfeldige tall innenfor et bestemt område.

```Rust
let random_number: u8 = rng.gen_range(1, 10);
```

Dette vil generere et tilfeldig tall mellom 1 og 10.

## Dype dykk:

Generering av tilfeldige tall har blitt brukt i programmering i mange år og er et viktig verktøy for å skape variasjon og sikkerhet. Det finnes også andre metoder for å generere tilfeldige tall, som for eksempel ved bruk av eksterne enheter som støygeneratorer eller til og med radiosignaler, men dette er mer avanserte og mindre vanlige metoder.

Når det kommer til implementering, er det viktig å bruke en pålitelig kilde for å generere de tilfeldige tallene, slik som ved bruk av "thread_rng" funksjonen i Rust. Det er også mulig å sette en frøverdi for å få samme sekvens av tilfeldige tall hver gang, men dette kan også gå ut over sikkerheten.

## Se også:

- [Rust Official Documentation on Random Number Generation](https://doc.rust-lang.org/std/rand/)
- [Understanding and Using Random Number Generators](https://www.computer.org/csdl/magazine/cg/2020/01/mcg2020010019/13Gm3P0MU1Z) (artikkel)
- [Cryptographically Secure Pseudo-Random Number Generators](https://www.random.org/randomness/) (artikkel og verktøy for å teste tilfeldig tallgeneratorer)