---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:03.719435-07:00
description: "Hur g\xF6r man: Google Apps Script, som bygger p\xE5 JavaScript, till\xE5\
  ter flera metoder f\xF6r att g\xF6ra om en str\xE4ng till stor bokstav i b\xF6rjan,\
  \ \xE4ven om det inte\u2026"
lastmod: '2024-03-13T22:44:37.419476-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, som bygger p\xE5 JavaScript, till\xE5ter flera metoder\
  \ f\xF6r att g\xF6ra om en str\xE4ng till stor bokstav i b\xF6rjan, \xE4ven om det\
  \ inte finns n\xE5gon inbyggd funktion f\xF6r detta."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur gör man:
Google Apps Script, som bygger på JavaScript, tillåter flera metoder för att göra om en sträng till stor bokstav i början, även om det inte finns någon inbyggd funktion för detta. Här är ett par kortfattade exempel:

**Metod 1: Använda charAt() och slice()**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// Exempelanvändning
let result = capitalizeString('hej, världen');
console.log(result);  // Utdata: Hej, världen
```

**Metod 2: Använda ett Regex**

För de som föredrar en regex-baserad lösning för att mer elegant hantera specialfall:

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// Exempelanvändning
let result = capitalizeStringRegex('hej, världen');
console.log(result);  // Utdata: Hej, världen
```

Båda metoderna säkerställer att den första bokstaven i strängen är stor och resten små, lämpliga för en mängd olika tillämpningar inklusive men inte begränsat till manipulation av Google Sheets eller dokumentredigering via Apps Script.

## Fördjupning
Att göra om strängar till stor bokstav i början i Google Apps Script är okomplicerat, genom att utnyttja JavaScripts kraftfulla strängmanipuleringsförmågor. Historiskt sett har språk som Python erbjudit inbyggda metoder som `.capitalize()` för att uppnå detta, vilket lägger till ett extra steg för JavaScript- och Apps Script-programmerare. Dock, frånvaron av en inbyggd funktion i JavaScript/Google Apps Script uppmuntrar till flexibilitet och en djupare förståelse för tekniker för strängmanipulering.

För mer komplexa scenarier, såsom att göra om varje ord i en sträng till stor bokstav i början (Titelstil), kan programmerare kombinera regex-metoder med `split()` och `map()` funktioner för att bearbeta varje ord individuellt. Även om Google Apps Script inte erbjuder en direkt metod för att göra om strängar till stor bokstav i början, ger användningen av befintliga JavaScript-strängmanipuleringsmetoder stor flexibilitet, vilket tillåter utvecklare att hantera strängar effektivt enligt deras specifika behov.

I fall där prestanda och effektivitet är av yttersta vikt, är det värt att notera att direkt strängmanipulering kan vara mer prestandaeffektiv än regex, särskilt för längre strängar eller operationer inom stora loopar. Dock, för de flesta praktiska tillämpningar inom Google Apps Script, erbjuder båda metoderna pålitliga lösningar.
