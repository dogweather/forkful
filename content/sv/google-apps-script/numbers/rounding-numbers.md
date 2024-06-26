---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:40.161345-07:00
description: "Hur: Google Apps Script, som \xE4r ett JavaScript-baserat spr\xE5k,\
  \ erbjuder standardmetoder f\xF6r att avrunda tal. H\xE4r \xE4r en uppdelning av\
  \ tre vanligt anv\xE4nda\u2026"
lastmod: '2024-03-13T22:44:37.432578-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, som \xE4r ett JavaScript-baserat spr\xE5k, erbjuder\
  \ standardmetoder f\xF6r att avrunda tal."
title: Avrundning av nummer
weight: 13
---

## Hur:
Google Apps Script, som är ett JavaScript-baserat språk, erbjuder standardmetoder för att avrunda tal. Här är en uppdelning av tre vanligt använda tekniker:

### Math.round()
Denna funktion avrundar ett tal till det närmaste heltalet.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Skriver ut: 3
```

### Math.ceil()
Avrundar ett tal uppåt till det närmaste heltalet.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Skriver ut: 3
```

### Math.floor()
Avrundar å andra sidan ett tal nedåt till det närmaste heltalet.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Skriver ut: 2
```

För specifika decimalplatser kan du använda `.toFixed()`, som faktiskt returnerar en sträng, eller ett mer nyanserat tillvägagångssätt för matematisk avrundning:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Skriver ut: "2.57" (som en sträng)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Skriver ut: 2.57
```

## Fördjupning
Att avrunda tal i Google Apps Script avviker inte mycket från hur det görs i andra JavaScript-miljöer. Dock är förståelsen för skillnaderna i avrundningsmetoder och potentialen för problem med flyttalsaritmetik avgörande. Till exempel, på grund av hur datorer representerar flyttal, kan inte alla decimalfraktioner representeras med perfekt noggrannhet, vilket leder till ibland oväntade avrundningsresultat.

Historiskt sett hanterar JavaScript (och därmed Google Apps Script) detta genom att följa IEEE 754-standarden, som används av många andra programmeringsspråk för flyttalsaritmetik. Denna standard definierar hur tal avrundas, vilket säkerställer konsekvens över olika plattformar och språk.

Medan direkta avrundningsmetoder i Google Apps Script är raka och ofta tillräckliga, kan komplexa eller högprecisionstillämpningar ha nytta av bibliotek som decimal.js eller big.js, som är designade för att hantera beräkningar med godtycklig precision. Dessa kan vara särskilt användbara när man arbetar med finansiella eller vetenskapliga beräkningar där avrundade tals noggrannhet är av yttersta vikt.

Kom ihåg dock att använda externa bibliotek i Google Apps Script kräver att de läses in genom scripteditorn, vilket kan införa beroenden eller påverka prestandan för ditt script beroende på hur det används. I många fall är de inbyggda Math-metoderna helt adekvata, men för de där marginalfallen som kräver precision till den n:te graden, kan det vara nödvändigt att se bortom standardbiblioteket.
