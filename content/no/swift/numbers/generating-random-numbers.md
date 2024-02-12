---
title:                "Generering av tilfeldige tall"
aliases:
- /no/swift/generating-random-numbers.md
date:                  2024-01-27T20:35:39.890380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall i programmering handler om å skape ikke-deterministiske eller uforutsigbare numeriske verdier. Programmerere bruker tilfeldige tall av mange grunner, som for eksempel å simulere uforutsigbarhet i spill, velge tilfeldige utvalg fra datasett, eller for kryptografiske formål.

## Hvordan:

Swift tilbyr en enkel måte å generere tilfeldige tall gjennom sitt standardbibliotek. Slik gjør du det for forskjellige numeriske typer:

```Swift
// Generer et tilfeldig heltall mellom 0 og Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Generer et tilfeldig flyttall mellom 0.0 og 1.0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Generer en tilfeldig Bool-verdi
let randomBool = Bool.random()
print(randomBool)
```

Eksempelutskriften kan variere fordi, vel, vi har tross alt å gjøre med tilfeldighet. Å kjøre koden flere ganger vil gi forskjellige tall og boolske verdier.

## Dypdykk

Swifts tilnærming til generering av tilfeldige tall er bygget på toppen av en robust og effektiv pseudotilfeldig tallgenerator (PRNG). Før Swift 4.2, stolte utviklere på eksterne biblioteker eller den underliggende plattformens evner, noe som kunne føre til inkonsekvenser på tvers av forskjellige plattformer og miljøer. Med introduksjonen av innfødte API-er i Swift 4.2, ble generering av tilfeldige tall både enklere og mer konsistent, uavhengig av den underliggende plattformen.

Det er imidlertid kritisk å forstå at den standard tilfeldige tallgeneratoren i Swift ikke er egnet for kryptografiske formål. For kryptografi, bør utviklere bruke `Security`-rammeverket på Apple-plattformer, som gir tilgang til kryptografisk sikre tilfeldige byte. Per min siste oppdatering, inkluderer ikke Swift en plattformuavhengig kryptografisk tilfeldig tallgenerator i sitt standardbibliotek, noe som presser utviklere til å søke etter tredjepartsbiblioteker for slike behov på ikke-Apple-plattformer.

I sfæren av vitenskapelig databehandling eller situasjoner som krever en deterministisk sekvens av pseudo-tilfeldige tall (der sekvensen kan reproduseres nøyaktig), er kanskje ikke Swifts generering av tilfeldige tall det beste valget uten muligheten til å så generatoren. I slike tilfeller brukes ofte spesialiserte biblioteker og algoritmer for å møte disse presise kravene.
