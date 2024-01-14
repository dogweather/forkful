---
title:    "TypeScript: Omvandla en sträng till gemener"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

##Varför
Det är ofta nödvändigt att konvertera en sträng till små bokstäver för att jämföra, sortera eller hantera data på ett enhetligt sätt.

##Hur man gör
```TypeScript
let sträng = "Hej Världen!";
console.log(sträng.toLowerCase());
```
Output: "hej världen!"

```TypeScript
let namn = "JONAS";
console.log(namn.toLowerCase());
```
Output: "jonas"

##Djupdykning
När du konverterar en sträng till små bokstäver i TypeScript, används metoden "toLowerCase ()" på strängobjektet. Denna metod returnerar en ny sträng med alla bokstäver i små bokstäver. Detta är användbart för att göra strängar mer konsistenta och lättlästa i kod. Det är också viktigt att komma ihåg att metoden bara konverterar bokstäver, inte siffror eller specialtecken.

Ett annat sätt att konvertera en sträng till små bokstäver är att använda operatören "===" för att jämföra strängen med dess egen konverterade version. Om de är identiska betyder det att strängen redan är i små bokstäver.

##Se även
- [Microsoft TypeScript dokumentation för toLowerCase ()](https://www.typescriptlang.org/docs/handbook/strings.html#lowercase-lowercase)
- [W3Schools - TypeScript toLowerCase () metod](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [Stack Overflow - How to convert a string to lower case in TypeScript?](https://stackoverflow.com/questions/37990024/how-to-convert-a-string-to-lower-case-in-typescript)