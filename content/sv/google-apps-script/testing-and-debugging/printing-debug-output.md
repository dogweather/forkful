---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:57.616005-07:00
description: "Att skriva ut felutskrifter inneb\xE4r att strategiskt placera logguttryck\
  \ i din kod f\xF6r att visa variabelv\xE4rden, utf\xF6randefl\xF6de eller felmeddelanden\
  \ under\u2026"
lastmod: '2024-03-13T22:44:37.441610-06:00'
model: gpt-4-0125-preview
summary: "Att skriva ut felutskrifter inneb\xE4r att strategiskt placera logguttryck\
  \ i din kod f\xF6r att visa variabelv\xE4rden, utf\xF6randefl\xF6de eller felmeddelanden\
  \ under k\xF6rning."
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

## Hur man gör:
Google Apps Script tillhandahåller klassen `Logger` för grundläggande felsökning, och för mer avancerade behov, klassen `console` introducerad i V8-runtime.

**Använda Logger:**

Logger-klassen låter dig logga felsökningsmeddelanden, vilka du kan se efter utförande i Apps Script Editor under `Visa > Loggar`. Här är ett enkelt exempel:

```javascript
function logSample() {
  var name = "Wired Reader";
  Logger.log("Hej, %s!", name);
}
```

Efter att ha kört `logSample()`, kan du se loggen med "Hej, Wired Reader!" i Loggvisaren.

**Använda console.log med V8-runtime:**

Med V8-runtime ger `console.log` en mer bekant syntax för utvecklare som kommer från andra språk:

```javascript
function consoleSample() {
  var status = 'aktiv';
  var antal = 150;
  console.log(`Nuvarande status: ${status}, Antal: ${antal}`);
}
```

Efter utförande, åtkom Stackdriver Logging i `Visa > Stackdriver Logging` för att se utskriften. Det är kraftfullare, stöder stränginterpolation och objektinspektion, och integreras med Google Clouds loggning, vilket erbjuder bestående loggar och avancerad filtrering.

**Exempelutskrift från console.log:**

```
Nuvarande status: aktiv, Antal: 150
```

## Fördjupning
I början var `Logger.log` det primära redskapet för felsökning i Google Apps Script, som erbjöd ett enkelt, rakt på sak sätt att skriva ut utskrifter för inspektion. Dock, när skript blev mer komplexa och integrerade med Google Cloud Platform-tjänster, blev behovet av en mer robust loggningslösning uppenbart.

Enter V8-runtime, som för in `console.log` i bilden. Detta inte bara linjerar Google Apps Script med standard JavaScript-syntax, vilket gör språket mer tillgängligt för utvecklare bekanta med JavaScript, men också utnyttjar den kraftfulla infrastrukturen hos Google Clouds loggningsmöjligheter. Introduktionen av `console.log` och dess integration med Google Cloud Platform markerar en betydande evolution i felsökningsförmågor inom Google Apps Script, vilket ger utvecklare en mer dynamisk och skalbar metod för att övervaka och felsöka sina skript.

Medan `Logger.log` är tillräckligt för grundläggande felsökningsbehov och små projekt, erbjuder `console.log` med V8-runtime en mer heltäckande och framtidsäker lösning. Detta inkluderar möjligheten att behålla loggar bortom utförandesessionen, söka och filtrera loggar inom Google Cloud-konsolen, och den allmänna anpassningen med moderna JavaScript-utvecklingspraxis. Dock bör utvecklare väga sina behov mot komplexiteten och skalan på sina projekt när de väljer mellan dessa alternativ.
