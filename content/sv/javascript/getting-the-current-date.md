---
title:                "Javascript: Att få den aktuella datumen"
simple_title:         "Att få den aktuella datumen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att få den aktuella datumen är en viktig del av programmering i Javascript. Det finns många användbara applikationer som kräver att du kan hämta och använda datumet, till exempel för att skapa tidstämplar eller visa för användaren när en viss händelse inträffade.

## Hur man gör
För att få den aktuella datumen i Javascript finns det flera olika metoder att använda. En enkel och vanlig metod är att använda `Date()`-funktionen. Den här funktionen returnerar ett objekt som innehåller den aktuella datumen och tiden.

```Javascript
const datum = new Date();
console.log(datum);

// Output: Thu Mar 11 2021 10:20:27 GMT+0100 (Central European Standard Time)
```

För att få mer specifika delar av datumet, använder du de inbyggda metoder som finns tillgängliga för `Date`-objektet. Till exempel kan du hämta året med `getFullYear()`, månaden med `getMonth()`, och dagen med `getDay()`.

```Javascript
const datum = new Date();
const år = datum.getFullYear();
const månad = datum.getMonth();
const dag = datum.getDay();
console.log(`${år}-${månad}-${dag}`);

// Output: 2021-3-4
```

## Djupdykning
För att vara mer exakt när det gäller tid och datum kan du använda `Intl`-objektet i Javascript. Detta objekt ger en mängd olika metoder som gör det möjligt att få den aktuella tiden och datumet baserat på det lokala språket och formatet.

Ett exempel på detta är `toLocaleDateString()` som returnerar det lokala datumet i ett specifikt format enligt användarens inställningar.

```Javascript
const datum = new Date();
const lokalDatum = datum.toLocaleDateString();
console.log(lokalDatum);

// Output: 4/3/2021 (för en användare i USA)
```

Det finns också möjlighet att använda `toLocaleTimeString()` för att få den lokala tiden i det önskade formatet, och även `toLocaleString()` för att få både datum och tid tillsammans.

## Se också
* [MDN web docs - Date](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
* [MDN web docs - Intl](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Intl)
* [W3Schools - JavaScript Date Objekt](https://www.w3schools.com/jsref/jsref_obj_date.asp)