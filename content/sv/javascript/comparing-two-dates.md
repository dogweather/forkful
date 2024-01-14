---
title:                "Javascript: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en viktig del av webbutveckling och kan hjälpa till att hantera datumbaserade uppgifter på ett effektivt sätt. Oavsett om du behöver jämföra datum för att sortera data eller för att kontrollera giltigheten för inmatade datum, är det en användbar kunskap att ha i din programmeringsverktygslåda.

## Så här gör du
Det finns flera olika sätt att jämföra två datum i Javascript, men det vanligaste är att använda "Date" objektet och dess inbyggda metoder. Här är ett exempel på hur du kan jämföra två datum och få ut ett resultat:

```javascript
let date1 = new Date('2021-01-01');
let date2 = new Date('2021-02-10');

if (date1 < date2) {
  console.log('Datum 1 kommer före datum 2');
} else if (date2 < date1) {
  console.log('Datum 2 kommer före datum 1');
} else {
  console.log('Datumen är samma');
}

// Output: Datum 1 kommer före datum 2
```

I det här exemplet skapas två "Date" objekt med hjälp av stränga representationer av datum. Sedan jämförs de två objekten med hjälp av en if-sats och resultatet loggas beroende på vilket datum som kommer före det andra.

En annan användbar metod för att jämföra datum är "getTime()", som returnerar antalet millisekunder sedan 1 januari 1970 för ett visst datum. Detta kan vara användbart om du behöver mer exakta jämförelser mellan datum.

## Djupdykning
Vid jämförelse av datum är det viktigt att förstå hur Javascript hanterar datum. Eftersom Javascript är ett språk baserat på objekt och funktioner, används "Date" objektet för att representera datum och tid. Men det finns några saker att tänka på när man jämför datum i Javascript:

- "Date" objektet använder det lokala tidszonen för användaren, så det är viktigt att tänka på detta när du arbetar med internationell data.
- När du skapar ett "Date" objekt med hjälp av en strängrepresentation av datum, måste formatet vara antingen "mm/dd/yyyy" eller "yyyy/mm/dd" för att det ska fungera korrekt i alla webbläsare.
- Det är också viktigt att notera att Javascripts inbyggda datumfunktioner är begränsade till datum mellan åren 0 och 9999, så om du behöver hantera datum utanför det intervallet måste du använda en tredjepartsbibliotek.

## Se även
- [MDN Web Docs: Date](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Luxon](https://moment.github.io/luxon/)