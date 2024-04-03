---
date: 2024-01-26 03:47:18.135346-07:00
description: "Avrundning av tal inneb\xE4r att trimma ett nummer till en specifik\
  \ precision. Programmerare g\xF6r detta f\xF6r att kontrollera numerisk utdata f\xF6\
  r l\xE4sbarhet,\u2026"
lastmod: '2024-03-13T22:44:37.651078-06:00'
model: gpt-4-0125-preview
summary: "Avrundning av tal inneb\xE4r att trimma ett nummer till en specifik precision."
title: Avrundning av tal
weight: 13
---

## Vad & Varför?
Avrundning av tal innebär att trimma ett nummer till en specifik precision. Programmerare gör detta för att kontrollera numerisk utdata för läsbarhet, visningssyften eller när specifik precision krävs efter operationer som ger flyttalsresultat.

## Hur man gör:
Avrundning i TypeScript kan göras med flera metoder. Här är en snabb genomgång:

```typescript
// Math.round avrunder till närmaste heltal
console.log(Math.round(1.5)); // Utdata: 2

// Math.ceil avrunder uppåt till närmaste heltal
console.log(Math.ceil(1.1)); // Utdata: 2

// Math.floor avrunder nedåt till närmaste heltal
console.log(Math.floor(1.8)); // Utdata: 1

// toFixed avrunder till ett fast antal decimaler
let num = 1.23456;
console.log(num.toFixed(2)); // Utdata: "1.23"
// Notera: toFixed returnerar en sträng! Använd parseFloat för att konvertera tillbaka om det behövs.
console.log(parseFloat(num.toFixed(2))); // Utdata: 1.23
```

## Djupdykning
Förr i tiden var avrundning ett måste på grund av begränsat utrymme och precisionssvårigheter i tidiga datorer. Idag kan flyttalsaritmetik leda till knepiga resultat på grund av hur tal lagras i binärt. Alternativ till avrundning inkluderar floor, ceil och trunc (för att hugga av decimaler utan avrundning).

Det är värt att notera interna funktioner: `Math.round` följer "avrunda halva upp" (även känd som "kommersiell avrundning"), medan `Math.floor` och `Math.ceil` är rakt på sak. `toFixed` kan orsaka oväntade resultat eftersom det returnerar en sträng, och det avrunder genom att använda "avrunda halva till jämnt" (även känt som "bankavrundning"), särskilt användbart för att minska partiskhet vid upprepade avrundningar av samma tal.

## Se även
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE-standarden för flyttalsaritmetik (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
