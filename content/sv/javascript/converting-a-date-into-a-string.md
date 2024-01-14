---
title:                "Javascript: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera ett datum till en sträng är en viktig del av Javascript-programmering. Det är användbart för att visa datum på ett sätt som är läsbart för människor och för att lagra datum i olika format.

## Hur man gör det
```Javascript
// Skapa ett datumobjekt
const date = new Date();

// Konvertera datumet till en sträng baserat på standarden "MM/DD/YYYY"
const dateString = date.toLocaleString('en-US', {dateStyle: 'short'});
console.log(dateString); // Output: 9/5/2021

// Konvertera datumet till en sträng baserat på standarden "YYYY-MM-DD"
const dateString2 = date.toISOString().substring(0, 10);
console.log(dateString); // Output: 2021-09-05

// Konvertera datumet till en anpassad sträng
const options = {weekday: 'long', year: 'numeric', month: 'long', day: 'numeric'};
const dateString3 = date.toLocaleDateString('sv-SE', options);
console.log(dateString3); // Output: söndag, 5 september 2021
```

## Djupdykning
När vi konverterar ett datum till en sträng säger vi egentligen att vi formatterar det. I Javascript finns det flera inbyggda metoder för att formatera datumobjekt, såsom `toLocaleString()` och `toISOString()`. Men det är också möjligt att skapa en anpassad formattering genom att ange olika parametrar i dessa metoder.

En annan viktig del av att konvertera datum till strängar är att förstå skillnaden mellan olika standarder för datumformatering, såsom "MM/DD/YYYY" eller "YYYY-MM-DD". Det är viktigt att välja rätt standard beroende på hur strängen kommer att användas eller sparas.

## Se också
- [MDN - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Date Formats](https://www.w3schools.com/js/js_date_formats.asp)
- [Javatpoint - JavaScript Date Methods](https://www.javatpoint.com/javascript-date-methods)