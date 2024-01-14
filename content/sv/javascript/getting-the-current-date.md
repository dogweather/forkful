---
title:    "Javascript: Att få den aktuella datumen"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Varför ska man använda sig av datum i Javascript?

Att använda och hantera datum i Javascript är inte alltid en uppenbar uppgift, men det är en viktig del av att skapa dynamiska och interaktiva webbplatser. Ofta kan det vara nödvändigt att visa eller jämföra datum, till exempel inom e-handelsapplikationer eller bokningssystem. I den här bloggposten ska vi titta på hur man kan använda Javascript för att få den nuvarande datumet och hantera det på olika sätt.

## Hur man får den nuvarande datumet i Javascript

För att få den nuvarande datumet i Javascript finns det ett inbyggt objekt som heter `Date()`. Det returnerar ett objekt med ett antal metoder för att hantera datum och tid. Här är ett exempel på hur man använder `Date()` för att hämta den nuvarande datumet och spara det i en variabel:

```Javascript
const nuvarandeDatum = new Date();
```

Vi kan också använda vissa metoder på detta objekt för att få specifika delar av datumet, till exempel året, månaden och dagen:

```Javascript
const året = nuvarandeDatum.getFullYear(); // returnerar hela året (ex: 2021)
const månaden = nuvarandeDatum.getMonth(); // returnerar månaden som ett nummer (0-11)
const dagen = nuvarandeDatum.getDate(); // returnerar dagen i månaden (1-31)
```

Om vi vill få datumet i ett visst format, till exempel "YYYY-MM-DD", kan vi använda oss av dessa värden och sätta ihop vår egna string med hjälp av till exempel `template literals`:

```Javascript
const datumSträng = `${året}-${månaden}-${dagen}`; // returnerar strängen "2021-08-05"
```

## Djupdykning i att hantera datum i Javascript

När det kommer till att hantera datum i Javascript finns det många olika metoder och bibliotek som kan användas för att göra det enklare. Till exempel finns det ett populärt bibliotek som heter "Moment.js" som har en mängd olika funktioner för att hantera och formatera datum. Det finns också inbyggda metoder som `toLocaleDateString()` som kan användas för att få datumet i olika regioner och språk.

Det finns också viktiga saker att tänka på när man hanterar datum i Javascript, som till exempel att datum ofta representeras på olika sätt i olika delar av världen. Det är också viktigt att vara medveten om tidszoner och hur de kan påverka datum och tid.

Att hantera datum i Javascript kan vara en utmaning, men det är en viktig del av varje utvecklares verktygslåda för att skapa dynamiska och funktionella webbplatser.

## Se även

- [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Working with dates in JavaScript](https://flaviocopes.com/javascript-dates/)