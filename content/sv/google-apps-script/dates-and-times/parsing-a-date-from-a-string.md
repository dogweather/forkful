---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:37.760770-07:00
description: "Hur: I Google Apps Script, som \xE4r baserat p\xE5 JavaScript, har du\
  \ flera tillv\xE4gag\xE5ngss\xE4tt f\xF6r att tolka ett datum fr\xE5n en str\xE4\
  ng. Nedan f\xF6ljer exempel som\u2026"
lastmod: '2024-03-13T22:44:37.449079-06:00'
model: gpt-4-0125-preview
summary: "I Google Apps Script, som \xE4r baserat p\xE5 JavaScript, har du flera tillv\xE4\
  gag\xE5ngss\xE4tt f\xF6r att tolka ett datum fr\xE5n en str\xE4ng."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur:
I Google Apps Script, som är baserat på JavaScript, har du flera tillvägagångssätt för att tolka ett datum från en sträng. Nedan följer exempel som använder både infödda JavaScript-metoder och verktyg från Google Apps Script.

**Använda `new Date()` konstruktören:**

Det enklaste sättet att tolka en sträng till ett datum i Google Apps Script är att använda `Date` objektets konstruktör. Det kräver dock att datumsträngen är i ett format som är igenkänt av Date.parse()-metoden (t.ex. ÅÅÅÅ-MM-DD).

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Loggar Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

**Använda `Utilities.parseDate()`:**

För mer flexibilitet, särskilt med anpassade datumformat, tillhandahåller Google Apps Script `Utilities.parseDate()`. Denna metod låter dig specificera datumformat, tidszon och locale (det regionala formatet).

```javascript
const dateString = '01-04-2023'; // DD-MM-ÅÅÅÅ
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // Loggar Sat Apr 01 2023 00:00:00 GMT+0000 (UTC) beroende på skriptets tidszon
```

Obs: Även om `Utilities.parseDate()` erbjuder mer kontroll, kan dess beteende variera baserat på skriptets tidszon, så det är avgörande att explicit ange tidszonen om din applikation hanterar datum över flera regioner.

## Fördjupning
Datumtolkning i programmeringsspråk har historiskt sett varit fyllt med utmaningar, främst på grund av mångfalden av datumformat och komplexiteten hos tidszoner. Google Apps Script's tillvägagångssätt, som huvudsakligen härstammar från JavaScript, syftar till att förenkla detta genom att erbjuda både det rättfram `Date` objektet och den mer mångsidiga funktionen `Utilities.parseDate()`. Dock har varje metod sina begränsningar; exempelvis leder beroendet på `Date` konstruktören med strängar till inkonsekvenser över olika miljöer på grund av olika tolkningar av datumformat. Å andra sidan kräver `Utilities.parseDate()` en tydligare förståelse för formatet, tidszonen och localet, vilket gör den något mer komplex men mer tillförlitlig för specifika behov.

Alternativa bibliotek eller tjänster, såsom Moment.js (som nu rekommenderar Luxon för nya projekt), tillhandahåller rikare funktionaliteter och bättre zonhantering och adresserar många av dessa utmaningar. Ändå, i sammanhanget av Google Apps Script, där externa bibliotek har begränsningar, blir det avgörande att förstå och använda de inbyggda metoderna effektivt. Programmerare som kommer från andra språk kan finna nyanserna av datumhantering i Google Apps Script unikt utmanande men kan uppnå robust datumtolkning med en djup förståelse för de tillgängliga verktygen och noggrann övervägning av deras applikationers globala natur.
