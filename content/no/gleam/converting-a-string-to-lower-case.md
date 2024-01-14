---
title:                "Gleam: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

**Gleam: Enkel måte å konvertere en streng til små bokstaver**

Har du noen gang lurt på hvordan du kan konvertere en stor bokstav til en liten bokstav i Gleam? I denne bloggposten vil vi vise deg en enkel måte å gjøre akkurat det!

## Hvorfor?

Du lurer kanskje på hvorfor det er nyttig å konvertere en streng til små bokstaver. Vel, det er flere grunner til dette. Kanskje du ønsker å sammenligne to strenger uten å ta hensyn til store og små bokstaver, eller kanskje du vil formatere en streng på en bestemt måte. Uansett hva grunnen er, kan det være nyttig å vite hvordan du enkelt kan konvertere en streng til små bokstaver i Gleam.

## Hvordan gjøre det?

For å konvertere en streng til små bokstaver i Gleam, kan du bruke funksjonen `String.to_lower()`. La oss se på et eksempel på hvordan dette kan gjøres:

```Gleam
let original_streng = "HELLO WORLD"
let konvertert_streng = String.to_lower(original_streng)
```

I dette eksempelet har vi en streng kalt `original_streng` som består av store bokstaver. Vi bruker deretter funksjonen `String.to_lower()` for å konvertere strengen til små bokstaver. Resultatet vil bli lagret i variabelen `konvertert_streng`. La oss se på hvordan dette ser ut når vi printer ut de to strengene:

```Gleam
io.format("Original streng: {}", [original_streng])
// Original streng: HELLO WORLD

io.format("Konvertert streng: {}", [konvertert_streng])
// Konvertert streng: hello world
```

Som du kan se, har funksjonen `String.to_lower()` enkelt konvertert alle store bokstaver til små bokstaver.

## Dypdykk

Du lurer kanskje på hvordan denne funksjonen fungerer under panseret. Vel, for å forklare det enkelt, så er det en del av standardbiblioteket i Gleam og er implementert ved hjelp av Unicode Standard. Dette betyr at funksjonen ikke bare tar hensyn til ASCII-tegn, men også andre språk og symboler som bruker Unicode. Dette gjør den svært allsidig og nyttig for å håndtere alle typer strenger.

## Se også

Nå som du vet hvordan du kan konvertere en streng til små bokstaver i Gleam, kan du prøve det ut selv. Husk å sjekke ut vår offisielle dokumentasjon for å lære mer om de ulike funksjonene og mulighetene i Gleam. Vi anbefaler også å ta en titt på disse ressursene for å forbedre dine Gleam-ferdigheter:

- [Gleam Dokumentasjon](https://gleam.run/documentation/)
- [Gleam Slack samfunn](https://gleam-lang.slack.com/)
- [Gleam Forum](https://elixirforum.com/c/gleam)

Vi håper denne bloggposten har vært nyttig og at du er klar til å konvertere strenger til små bokstaver i Gleam! Lykke til med å utforske dette kraftfulle programmeringsspråket.