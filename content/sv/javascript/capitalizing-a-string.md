---
title:                "Gör om en sträng till versaler"
html_title:           "Javascript: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kapitalisera en sträng i programmering innebär att ändra alla bokstäver i strängen till stora bokstäver. Programmerare gör detta för att förbättra läsbarheten och för att standardisera textdata.

## Så här gör du:

I Javascript kan du kapitalisera en sträng med `toUpperCase()` funktionen. Det är en inbyggd funktion i JavaScript som gör alla bokstäver i en sträng till stora bokstäver.

Här är några exempel:

```Javascript
let myString = "hej världen";
let upperCaseString = myString.toUpperCase();

console.log(upperCaseString);
```

Output:

```Javascript
"HEJ VÄRLDEN"
```

## Djupdykning

Historiskt sett har skapandet av kapitalbokstäver varit en enkel uppgift eftersom ASCII-värdet för bokstäver skiljer sig med 32 enheter mellan små och stora bokstäver. Moderna JavaScript-motorer implementerar metoder som `toUpperCase()` för att underlätta för oss.

Ett alternativ till `toUpperCase()` är att iterera genom varje tecken i strängen och ändra det manuellt. Men det är mer komplicerat och ineffektivt.

JavaScript-motorn bakom `toUpperCase()` tar hand om många detaljer som vi annars skulle behöva hantera själva, som diakritiska tecken och dubbelbyte-bokstäver.

Se även metoden `toLocaleUpperCase()` som tar hänsyn till inställningarna för den aktuella platsen för att omvandla tecken till versaler.

## Se också

1. [Mozillas `toUpperCase()` dokumentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase) 

2. [Jag vill veta mer om `toLocaleUpperCase()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)

3. [Mer information om ASCII-tabellen](http://www.asciitable.com/)