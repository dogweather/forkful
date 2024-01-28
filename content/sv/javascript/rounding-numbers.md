---
title:                "Avrundning av tal"
date:                  2024-01-26T03:45:42.849436-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad och varför? 
Avrundning innebär att skära av "bruset" efter en viss punkt i ett tal. Programmerare avrundar för att kontrollera precision, hantera minne eller göra utskriften användarvänlig—som att göra om 2.998 till ett rent 3.

## Hur man gör:
Så här avrundar du tal i JavaScript med `Math.round()`, `Math.ceil()`, och `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (eftersom .567 är mer än .5)

console.log(roundedDown); // Skriver ut: 2
console.log(roundedUp);   // Skriver ut: 3
console.log(rounded);     // Skriver ut: 3
```

För att fixera till ett visst antal decimaler, använd `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (returnerar en sträng)

console.log(twoDecimals); // Skriver ut: "2.57"
```

Konvertera strängen tillbaka till ett nummer med ett unärt plus eller `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Skriver ut: 2.57
```

## Fördjupning
Avrundning av tal är inte nytt; det är lika gammalt som talen själva. I JavaScript använder `Math.round()` "avrunda halvupp"-avbrytning: om den fraktionella delen är 0.5, avrundas det till det närmaste jämna talet.

För mer kontroll kan `toFixed()` vara din lösning, men kom ihåg att det returnerar en sträng. Att konvertera tillbaka till ett nummer kan vara ett extra steg men säkerställer att du fortsätter arbeta med numeriska typer.

Alternativ? Bibliotek som `lodash` erbjuder `_.round(number, [precision=0])` för mer nyanserad styrning. Eller så ger den nyare `Intl.NumberFormat` dig hög precision formattering utöver bara avrundning.

När det gäller precision, var medveten om flyttalskonstigheter i JavaScript. `0.1 + 0.2` blir inte exakt `0.3` på grund av hur tal lagras. Ibland blir avrundning nödvändig för att korrigera sådana flyttalsfel.

## Se även
- Mozillas matte dokumentation: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Finansiell avrundning med `Intl.NumberFormat`: [ECMAScript Internationalization API](https://tc39.es/ecma402/#numberformat-objects)
- Avrundning i `lodash`: [Lodash Docs](https://lodash.com/docs/4.17.15#round)
