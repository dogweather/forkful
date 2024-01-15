---
title:                "Att hitta strängens längd"
html_title:           "Javascript: Att hitta strängens längd"
simple_title:         "Att hitta strängens längd"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att veta längden på en sträng är en grundläggande funktion inom programmering, speciellt inom språket Javascript. Det kan vara användbart för att hantera textinput, validera data och för att skapa dynamiska funktioner.

## Hur man gör
För att få längden på en sträng i Javascript, används funktionen `.length` tillsammans med variabeln som innehåller strängen.

```Javascript
// Skapa en variabel som innehåller en sträng
var namn = "Anna";

// Använd .length för att få längden på strängen
console.log(namn.length); // Output: 4
```

Om du vill kunna räkna med antalet tecken i en sträng utan mellanslag, kan du använda `.replace()` metoden tillsammans med en reguljäruttryck (RegExp) för att ta bort mellanslag från strängen.

```Javascript
// Skapa en sträng med mellanslag
var mening = "Det är en vacker dag";

// Använd .replace() metoden för att ta bort mellanslag med hjälp av reguljäruttrycket /\s/g
var renMening = mening.replace(/\s/g,"");

// Använd .length för att få längden på den rena strängen
console.log(renMening.length); // Output: 14
```

## Djupdykning
När du använder funktionen `.length` på en variabel som inte innehåller en sträng, kommer Javascript att automatiskt konvertera variabeln till en sträng och sedan räkna antalet tecken.

En annan sak att tänka på är att `.length` funktionen räknar både siffror och bokstäver. Så om du har en variabel med värdet `1234`, kommer längden att bli 4 trots att det egentligen bara är en siffra. 

Du kan också använda `.length` i en loop för att gå igenom varje tecken i en sträng och göra viss manipulation av data baserat på dess position. Detta kan vara användbart när du arbetar med stora dataset eller om du vill använda specifika delar av en sträng.

## Se även
- [String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length) (engelska)
- [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) (engelska)