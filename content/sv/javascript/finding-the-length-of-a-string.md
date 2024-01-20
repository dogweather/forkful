---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng i JavaScript är liktydigt med att beräkna antalet tecken i den. Detta är oftast nödvändigt när vi behöver iterera genom varje tecken i strängen eller begränsa inmatningen från användaren.

## Hur man gör:

```Javascript
let str = "Hej Sverige";
console.log(str.length);  // Utdata: 11
```
I ovanstående kod får vi längden på strängen 'Hej Sverige' genom att använda `.length` egenskapen.

```Javascript
let str = "";
console.log(str.length);  // Utdata: 0
```
För en tom sträng är längden alltid 0.

```Javascript
let str = " ";
console.log(str.length);  // Utdata: 1
```
Observera att blanksteg också räknas som ett tecken. 

## Djupdykning

Historiskt sett har `length`-egenskapen i JavaScript alltid varit ett enkelt och effektivt sätt att mäta längden på en sträng. Men eftersom språket har utvecklats har det tillkommit andra metoder, till exempel genom att använda 'Array.from()'. I det här fallet konverteras strängen till en array och sedan används `.length`-egenskapen.

```Javascript
let str = "Hej Sverige";
console.log(Array.from(str).length);  // Utdata: 11
```
Observera dock att denna metod är mer tidskrävande än den första metoden och bör endast användas när kompatibilitet med äldre browsers är nödvändigt.

När det kommer till implementation, lagras strängens längd internt i JavaScript-motorer för att snabbt hämta det när `.length`-egenskapen kallas. 

## Se även

För mer information och alternativa metoder, se följande resurser:

- [Mozilla Developer Network (MDN) - String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [JavaScript.info - String](https://javascript.info/string)
- [W3Schools - JavaScript String length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)