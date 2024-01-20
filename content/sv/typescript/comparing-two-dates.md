---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum innebär att avgöra om ett datum är tidigare, samma, eller senare än ett annat datum. Programmerare gör detta för att utföra tidsbaserade operationer, exempelvis för att spåra händelseordning eller beräkna tidsskillnad.

## Hur man gör:
Här är ett exempel på hur man jämför två datum i TypeScript:

```TypeScript
let datum1 = new Date('2022-01-01');
let datum2 = new Date('2022-12-31');

if(datum1 > datum2){
  console.log('Datum1 är senare än Datum2');
} else if(datum1 < datum2){
  console.log('Datum1 är tidigare än Datum2');
} else {
  console.log('Datum1 är samma som Datum2');
}
```

I detta exempel kommer output att bli 'Datum1 är tidigare än Datum2'.

## Djupdykning
Funktionen att jämföra datum introducerades tidigt i programmeringsspråk, framförallt på grund av dess betydelse för organisering och hantering av data baserad på tidsstämplar.

Som alternativ kan du använda bibliotek som moment.js för att jämföra datum, men JavaScript och TypeScript har inbyggd funktionalitet som fungerar lika bra.

När datumobjekt jämförs i JavaScript och TypeScript, omvandlas de faktiskt till timestamp (antal millisekunder sedan 1 januari 1970) innan jämförelsen äger rum. Detta är detaljen som möjliggör jämförelseoperationerna.

## Se även
För vidare läsning om hantering av datum i TypeScript, se följande källor:

- [Date - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)

Observera att jämföra datum kan vara mer komplicerat i vissa situationer, som att hantera tidszoner och skottår. Var noga med att även förstå dessa koncept.