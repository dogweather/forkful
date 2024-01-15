---
title:                "Kapitalisera en sträng"
html_title:           "Javascript: Kapitalisera en sträng"
simple_title:         "Kapitalisera en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Varför:
Att kapitalisera en sträng kan vara användbart för att förbättra utseendet på en webbsida eller för att underlätta läsbarheten av viss text. Det är också en vanlig formateringspraxis i många programmeringsspråk.

##Så här gör du det:
Det finns olika sätt att kapitalisera en sträng i Javascript, beroende på vilket användningsområde du har. Här är några exempel på kod som kan användas:

```Javascript
//Exempel 1: Kapitalisera alla bokstäver i en sträng
let str = "detta är en sträng";
str = str.toUpperCase();
console.log(str); // Output: DETTA ÄR EN STRÄNG

//Exempel 2: Kapitalisera första bokstaven i varje ord i en sträng
let str = "hej där!";
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}
console.log(capitalize(str)); // Output: Hej Där!

//Exempel 3: Kapitalisera första bokstaven i en mening
let str = "jag älskar javascript";
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}
console.log(capitalize(str)); // Output: Jag älskar javascript
```

##Djupdykning:
Javascript har inbyggda funktioner som tillåter oss att enkelt manipulera strängar, inklusive att ändra storleken på bokstäver. I exemplet ovan använde vi funktionen "toUpperCase()" för att ändra alla bokstäver till stora bokstäver. Men det finns också möjlighet att använda funktionen "toLowerCase()" för att göra alla bokstäver små.

En annan användbar funktion för att kapitalisera strängar är "charAt()", som returnerar tecknet vid en given position i en sträng. Genom att kombinera denna funktion med "toUpperCase()" eller "toLowerCase()", kan vi ändra bara en bokstav i en sträng.

Det finns också olika bibliotek och ramverk tillgängliga för Javascript som gör det enklare att manipulera och formatera strängar på olika sätt. Några exempel är string.js, lodash och jQuery.

##Se även:
- [toUpperCase() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [toLowerCase() - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [string.js](http://stringjs.com/)
- [lodash](https://lodash.com/)
- [jQuery](https://jquery.com/)