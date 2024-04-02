---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:03.789409-07:00
description: "Att ber\xE4kna ett datum i framtiden eller det f\xF6rflutna handlar\
  \ om att manipulera datumobjekt f\xF6r att hitta datum bortom eller f\xF6re nuvarande\
  \ datum,\u2026"
lastmod: '2024-03-13T22:44:37.453464-06:00'
model: gpt-4-0125-preview
summary: "Att ber\xE4kna ett datum i framtiden eller det f\xF6rflutna handlar om att\
  \ manipulera datumobjekt f\xF6r att hitta datum bortom eller f\xF6re nuvarande datum,\u2026"
title: "Ber\xE4kning av ett datum i framtiden eller f\xF6rflutet"
weight: 26
---

## Vad & Varför?

Att beräkna ett datum i framtiden eller det förflutna handlar om att manipulera datumobjekt för att hitta datum bortom eller före nuvarande datum, respektive. Programmerare gör detta för uppgifter som sträcker sig från att ställa in påminnelser och utgångsdatum till att analysera trender i tidsbaserade data.

## Hur man gör:

I Google Apps Script, som är baserat på JavaScript, kan du manipulera datum med hjälp av `Date`-objektet. Så här beräknar du datum i framtiden och det förflutna:

### Beräkning av framtida datum

För att beräkna ett framtida datum skapar du ett datumobjekt för det aktuella datumet och lägger sedan till önskat antal dagar (eller någon annan tidsenhet) till det.

```javascript
// Aktuellt datum
var today = new Date();

// Beräkna ett datum 10 dagar i framtiden
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Framtida Datum: " + futureDate.toDateString());
```

### Beräkning av tidigare datum

På samma sätt, för att hitta ett datum i det förflutna, subtrahera antalet dagar från det aktuella datumet.

```javascript
// Aktuellt datum
var today = new Date();

// Beräkna ett datum 10 dagar i det förflutna
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Tidigare Datum: " + pastDate.toDateString());
```

### Exempel på utskrift

Detta skulle ge något i stil med följande (om vi antar att idag är den 15 april 2023):

```
Framtida Datum: Tis Apr 25 2023
Tidigare Datum: Ons Apr 05 2023
```

Kom ihåg, att `Date`-objektet i JavaScript (och därmed i Google Apps Script) automatiskt justerar månader och år när du lägger till eller drar ifrån dagar.

## Fördjupning

Manipuleringen av datum med hjälp av `Date`-objektet härstammar från tidiga JavaScript-implementeringar. Över tid har denna metod generellt sett förblivit konsekvent, vilket ger en enkel väg för utvecklare att hantera datum utan att behöva externa bibliotek. Dock, för mer komplexa operationer som tidszonsjusteringar, eller när man arbetar med omfattande datummässiga data, kan bibliotek som `Moment.js` eller det mer moderna `Luxon` erbjuda mer funktionalitet och enklare hantering.

I Google Apps Script, specifikt, trots den direkta tillgängligheten och enkelheten hos `Date`-objektet, är det avgörande att vara medveten om hur datumberäkningar kan påverka skriptets prestanda och exekveringstid, särskilt i tidsdrivna triggers eller omfattande kalkylbladsmanipulationer. Dessutom, även om Google Apps Script ger inbyggda metoder för att hantera datum inom sitt ekosystem (som i Google Kalkylark eller Kalender), kan integrering av externa bibliotek eller användande av Googles avancerade tjänster ibland erbjuda robustare lösningar för komplexa scenarier.

Så, även om den inbyggda JavaScript `Date`-objektmetodiken vanligtvis är tillräcklig för raka beräkningar, kan utforskande av externa bibliotek eller tjänster förbättra funktionaliteten för mer nyanserade krav.
