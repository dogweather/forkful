---
title:                "Generering av tilfeldige tall"
date:                  2024-01-27T20:33:25.345906-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Generering av tilfeldige tall i programmering kan være avgjørende for å skape simuleringer, testing, kryptografi og spill. I Gleam er det en funksjon som lar utviklere innføre uforutsigbarhet eller simulere virkelige scenarier i deres applikasjoner.

## Hvordan:

For å generere tilfeldige tall i Gleam bruker du primært `gleam_random`-biblioteket. Dette biblioteket gir funksjoner for å generere tilfeldige heltall, flyttall og mer. Først, sørg for at du har lagt til `gleam_random` i din `rebar.config` eller `mix.exs`-fil som en avhengighet.

La oss dykke inn i noen eksempler:

### Generere et tilfeldig heltall

For å produsere et tilfeldig heltall innenfor et spesifisert område, kan du bruke `int`-funksjonen:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Denne funksjonen vil generere et tilfeldig heltall mellom 1 og 10 inklusive.

### Generere et tilfeldig flyttall

For å få et tilfeldig flyttall, bruk `float`-funksjonen. Dette genererer et flyttall mellom 0,0 og 1,0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Eksempel på output

Å kjøre disse funksjonene kan gi output som:

- For `generate_random_int()`: `5`
- For `generate_random_float()`: `0,84372`

Husk, hver utførelse kan føre til forskjellige outputs på grunn av tilfeldighetens natur.

## Dypdykk

`gleam_random`-modulen implementerer en pseudotilfeldig tallgenerator (PRNG), som i hovedsak betyr at tallene ikke er helt tilfeldige, men er vanskelige å forutsi, noe som etterligner tilfeldighet. PRNGer opererer ved å starte med en initialverdi, kjent som en seed, og anvender matematiske operasjoner for å generere en sekvens av tall.

Historisk har språk og biblioteker implementert flere algoritmer for PRNGer, som Mersenne Twister eller Lineær Kongruensiell Generator (LCG). Valget av algoritme påvirker kvaliteten på "tilfeldigheten", med noen som er mer egnet for kryptografiske applikasjoner enn andre. Mens Gleams standardbibliotek gir bekvemmelighet og brukervennlighet med sin `gleam_random`-modul, er det kanskje ikke alltid det beste valget for brukssaker som krever kryptografisk sikker tilfeldighet. For kryptografiske formål bør utviklere se på biblioteker spesifikt designet for å tilby kryptografisk sikre pseudotilfeldige tallgeneratorer (CSPRNGer), som er designet for å motstå angrep som kunne forutsi fremtidige tall basert på å observere en sekvens av genererte tall.

I konklusjonen, mens Gleams funksjonalitet for generering av tilfeldige tall er robust for generelle programmeringsbehov, bør applikasjoner med spesifikke sikkerhetskrav vurdere dedikerte kryptografiske løsninger for å sikre integriteten og sikkerheten til deres tilfeldige tallgenerering.
