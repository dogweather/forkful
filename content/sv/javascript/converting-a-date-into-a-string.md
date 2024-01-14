---
title:    "Javascript: Att omvandla ett datum till en sträng"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

När man arbetar med datum och tid i Javascript, så är det ofta nödvändigt att konvertera ett datum till en sträng - detta kan handla om att visa datumet i ett visst format eller att jämföra datum och tider. Att kunna konvertera ett datum till en sträng är en viktig färdighet för alla Javascript-utvecklare.

## Hur man gör det

Det finns flera sätt att konvertera ett datum till en sträng i Javascript. En vanlig metod är att använda `toLocaleString()` -funktionen som finns inbyggd i JavaScript Date objektet. Denna funktion tar ett argument för önskat språk och returnerar datumet som en sträng baserad på det språket. Ett exempel på hur man skulle använda `toLocaleString()` för att konvertera ett datum till en sträng på svenska skulle se ut såhär:

```javascript
let datum = new Date();
let sträng = datum.toLocaleString("sv-SE");
console.log(sträng);
// Output: torsdag 16 juli 2020 11:45:13
```

Notera att strängen som returneras är baserad på det aktuella datumet och tiden vid körning av koden.

En annan metod för att konvertera ett datum till en sträng är att använda `toString()` -funktionen. Denna funktion returnerar också ett datum som en sträng, men i ett fast format oavsett vilket språk eller geografisk plats som används.

```javascript
let datum = new Date();
let sträng = datum.toString();
console.log(sträng);
// Output: Thu Jul 16 2020 11:45:13 GMT+0200 (Central European Summer Time)
```

Notera att formatet på den returnerade strängen kan variera beroende på användarens inställningar och browser.

## Djupdykning

Även om `toLocaleString()` och `toString()` är två vanliga metoder för att konvertera ett datum till en sträng, så finns det fler sätt att göra det på. Om man vill ha mer kontroll över formatet på den returnerade strängen, så kan man använda `getDate()`, `getMonth()` och `getFullYear()` -funktionerna för att hämta det önskade datumet, månaden och året och sedan kombinera dem i en sträng på det önskade sättet. Ett exempel på detta skulle kunna se ut såhär:

```javascript
let datum = new Date();
let dag = datum.getDate();
let månad = datum.getMonth() + 1;
let år = datum.getFullYear();
let sträng = dag + "/" + månad + "/" + år;
console.log(sträng);
// Output: 16/7/2020
```

Genom att använda dessa inbyggda funktioner och metoder i Javascript, så kan man enkelt konvertera ett datum till en sträng på det sätt som passar bäst för ens egna behov.

## Se även

- [MDN web docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript.info - Date and time](https://javascript.info/date)