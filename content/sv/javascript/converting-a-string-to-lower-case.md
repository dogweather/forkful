---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till gemena bokstäver innebär att ändra alla stora bokstäver i en sträng till små bokstäver. Programmerare brukar göra det när de vill jämföra strängar utan att ta hänsyn till teckens case, dvs om en bokstav är stor eller liten.

## Så här gör du:

I JavaScript, använd metoden `toLowerCase()` för att göra om alla bokstäver i en sträng till små bokstäver. Det ser ut så här:

```Javascript
let str = "Hej Världen!";
let lowerCaseStr = str.toLowerCase();
console.log(lowerCaseStr); // utskrift: "hej världen!"
```

## Djupdykning:

`toLowerCase()`-metoden har funnits sedan de första versionerna av JavaScript och är en del av ECMAScript-standarderna. Det finns alternativ, som `toLocaleLowerCase()`, som kan vara bättre om du jobbar med strängar som innehåller tecken som är specifika för vissa språk.

Metoden `toLowerCase()` går igenom varje tecken i strängen, checkar om det är en stor bokstav och omvandlar den sedan till en liten bokstav. Allting annat, inklusive siffror och specialtecken, lämnas som de är.

## Se även:

För mer information, se följande resurser:

- [Mozilla Developer Network's toLowerCase() Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScript.info’s guide to JavaScript strings](https://javascript.info/string) 

Kom ihåg, ett bra program är ett flexibelt program. Använda rätt verktyg för rätt jobb, och låt dina strängar uttrycka sig själva på det sätt som passar bäst för ditt projekt.