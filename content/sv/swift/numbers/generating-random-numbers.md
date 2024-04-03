---
date: 2024-01-27 20:35:34.306766-07:00
description: "Hur man g\xF6r: Swift erbjuder ett enkelt s\xE4tt att generera slumpm\xE4\
  ssiga tal genom sitt standardbibliotek. S\xE5 h\xE4r g\xF6r du det f\xF6r olika\
  \ numeriska typer."
lastmod: '2024-03-13T22:44:38.247013-06:00'
model: gpt-4-0125-preview
summary: "Swift erbjuder ett enkelt s\xE4tt att generera slumpm\xE4ssiga tal genom\
  \ sitt standardbibliotek."
title: Generera slumptal
weight: 12
---

## Hur man gör:
Swift erbjuder ett enkelt sätt att generera slumpmässiga tal genom sitt standardbibliotek. Så här gör du det för olika numeriska typer:

```Swift
// Generera ett slumpmässigt heltal mellan 0 och Int.max
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// Generera ett slumpmässigt flyttal mellan 0.0 och 1.0
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// Generera ett slumpmässigt Bool-värde
let randomBool = Bool.random()
print(randomBool)
```

Exempelresultat kan variera eftersom, ja, vi har att göra med slumpmässighet efter allt. Att köra koden flera gånger kommer att ge olika nummer och booleska värden.

## Fördjupning
Swifts tillvägagångssätt för att generera slumpmässiga tal bygger på en robust och effektiv pseudoslumptalsgenerator (PRNG). Före Swift 4.2 förlitade sig utvecklare på externa bibliotek eller den underliggande plattformens förmågor, vilket kunde leda till inkonsekvenser över olika plattformar och miljöer. Med introduktionen av inbyggda API:er i Swift 4.2 blev generering av slumpmässiga tal både enklare och mer konsekvent, oavsett underliggande plattform.

Det är dock kritiskt att förstå att den standard slumpmässig talgenerator i Swift inte är lämplig för kryptografiska ändamål. För kryptografi bör utvecklare använda `Security`-ramverket på Apple-plattformar, som ger tillgång till kryptografiskt säkra slumpmässiga bytes. I min senaste uppdatering inkluderar Swift inte en plattformsoberoende kryptografisk slumpmässig talgenerator i sitt standardbibliotek, vilket tvingar utvecklare att söka efter tredjepartsbibliotek för sådana behov på icke-Apple-plattformar.

Inom området för vetenskaplig databehandling eller situationer som kräver en deterministisk sekvens av pseudo-slumpmässiga tal (där sekvensen kan reproduceras exakt), kanske Swifts slumpmässiga talgenerering inte är det bästa alternativet utan möjligheten att sådda generatoren. I sådana fall används ofta specialiserade bibliotek och algoritmer för att uppfylla dessa precisa krav.
