---
title:    "TypeScript: Jämförelse av två datum"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara en mycket användbar funktion inom programmering. Genom att jämföra datum kan vi hitta skillnader och utföra olika åtgärder baserat på resultaten. I den här bloggposten kommer vi att titta på hur man kan jämföra två datum i TypeScript.

## Hur man gör
För att jämföra två datum i TypeScript behöver vi först skapa två variabler som innehåller de önskade datumerna. Detta kan göras på olika sätt, till exempel genom att använda Date-objektet eller Date.parse() funktionen. Här är ett exempel på hur man deklarerar två variabler och ger dem värden med hjälp av Date.parse() funktionen:

```TypeScript
let date1: Date = new Date(Date.parse("2021-01-01"));
let date2: Date = new Date(Date.parse("2020-12-25"));
```

För att jämföra dessa datum kan vi använda JavaScripts inbyggda metoder, som getTime() eller valueOf(), för att få ut ett numeriskt värde av varje datum. Vi kan sedan använda någon vanlig jämförelseoperator, som till exempel "===" eller ">=" för att jämföra de numeriska värdena. Här är ett exempel på hur detta kan göras:

```TypeScript
if (date1.getTime() === date2.getTime()) {
  console.log("Datumen är lika.");
} else if (date1.getTime() > date2.getTime()) {
  console.log("Datum1 är senare än Datum2.");
} else {
  console.log("Datum1 är tidigare än Datum2.");
}
```

Resultatet av detta kodblock kommer att vara "Datum1 är senare än Datum2".

## Djupdykning
Det finns många olika metoder för att jämföra datum i TypeScript, beroende på dina specifika behov och önskemål. En annan vanlig metod är att använda JavaScripts inbyggda Date-objekt, som har ett antal metoder för att jämföra olika delar av datum, som till exempel year, month och day.

En annan aspekt att tänka på när du jämför datum är att se till att de är i samma tidszon. Olika tidszoner kan orsaka felaktiga jämförelser om man inte tar hänsyn till dem.

## Se även
Här är några andra användbara länkar för att lära dig mer om att jämföra datum i TypeScript:

- [Microsoft Docs - Date Object](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Date Object](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [TypeScript Tutorial - Compare Date](https://www.typescripttutorial.net/typescript-tutorial/compare-dates/)

Tack för att du läste denna bloggpost om att jämföra datum i TypeScript. Vi hoppas att den varit användbar för dig!