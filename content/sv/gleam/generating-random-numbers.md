---
title:                "Generera slumptal"
date:                  2024-01-27T20:33:36.576835-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generera slumptal"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga tal i programmering kan vara avgörande för att skapa simulationer, tester, kryptografi och spel. I Gleam är det en funktion som låter utvecklare introducera oförutsägbarhet eller simulera verkliga scenarier i deras applikationer.

## Hur man gör:

För att generera slumpmässiga tal i Gleam använder du främst `gleam_random`-biblioteket. Detta bibliotek tillhandahåller funktioner för att generera slumpmässiga heltal, flyttal och mer. Först, se till att du har lagt till `gleam_random` i din `rebar.config` eller `mix.exs`-fil som ett beroende.

Låt oss dyka in i några exempel:

### Generera ett slumpmässigt heltal

För att producera ett slumpmässigt heltal inom ett specificerat intervall kan du använda funktionen `int`:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Denna funktion kommer att generera ett slumpmässigt heltal mellan 1 och 10 inklusive.

### Generera ett slumpmässigt flyttal

För att få ett slumpmässigt flyttal, använd funktionen `float`. Detta genererar ett flyttal mellan 0,0 och 1,0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Exempel på utdata

Körning av dessa funktioner kan ge utdata som:

- För `generate_random_int()`: `5`
- För `generate_random_float()`: `0,84372`

Kom ihåg, varje körning kan leda till olika utdata på grund av slumpmässighetens natur.

## Djupdykning

Modulen `gleam_random` implementerar en pseudoslumpmässig nummergenerator (PRNG), vilket i grunden betyder att talen inte är verkligt slumpmässiga men är svåra att förutsäga, och på så sätt efterliknar slumpmässighet. PRNG:er fungerar genom att starta med ett initialt värde, känt som fröet, och tillämpa matematiska operationer för att generera en sekvens av tal.

Historiskt sett har språk och bibliotek implementerat flera algoritmer för PRNG:er, som Mersenne Twister eller Linear Congruential Generator (LCG). Valet av algoritm påverkar kvaliteten på "slumpmässigheten", med vissa som är mer lämpliga för kryptografiska tillämpningar än andra. Medan Gleams standardbibliotek erbjuder bekvämlighet och enkel användning med sin `gleam_random`-modul, kanske det inte alltid är det bästa valet för användningsfall som kräver kryptografiskt säker slumpmässighet. För kryptografiska ändamål bör utvecklare undersöka bibliotek som specifikt är utformade för att tillhandahålla kryptografiskt säkra pseudoslumpmässiga nummergeneratorer (CSPRNGs), vilka är utformade för att motstå attacker som skulle kunna förutse framtida tal baserat på att observera en sekvens av genererade tal.

Sammanfattningsvis, även om Gleams funktionalitet för generering av slumpmässiga tal är robust för allmänna programmeringsbehov, bör applikationer med specifika säkerhetskrav överväga dedikerade kryptografiska lösningar för att säkerställa integriteten och säkerheten i sin generering av slumpmässiga tal.
