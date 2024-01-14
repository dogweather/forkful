---
title:    "Javascript: Omvandla en sträng till små bokstäver"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig uppgift inom Javascript-programmering. Det gör det lättare att jämföra strängar och söka igenom dem utan att behöva oroa sig för skillnaden mellan stora och små bokstäver. Genom att lära dig hur man konverterar en sträng till små bokstäver kan du effektivisera ditt arbete och göra din kod mer lättläst och robust.

## Hur man gör

För att konvertera en sträng till små bokstäver i Javascript kan du använda funktionen `toLowerCase()`. Här är ett enkelt exempel:

```Javascript
let str = "Hej! Detta Är En Sträng";
let lowerCaseStr = str.toLowerCase();

console.log(lowerCaseStr); // outputs "hej! detta är en sträng"
```

Som du kan se används `toLowerCase()` för att ändra alla bokstäver i en sträng till små bokstäver. Detta fungerar även för speciella tecken och bokstäver med accent.

En annan metod för att konvertera en sträng till små bokstäver är att använda metoden `replace()` i kombination med en regular expression. Här är ett exempel på hur det kan se ut:

```Javascript
let str = "Du borde läRA dig hur man KONVERterar En strÄNG TILL Små Bokstäver!";
let lowerCaseStr = str.replace(/[A-ZÄ-Ö]/g, function(match) {
  return match.toLowerCase();
});

console.log(lowerCaseStr); // outputs "du borde lära dig hur man konverterar en sträng till små bokstäver!"
```

I detta exempel används en regular expression för att matcha alla stora bokstäver och bokstäver med accent. Därefter används funktionen `toLowerCase()` för att ändra matchningen till små bokstäver.

## Djupdykning

Det finns flera olika anledningar till varför man skulle vilja konvertera en sträng till små bokstäver i en Javascript-applikation. En av de vanligaste är för jämförelseändamål. Om du t.ex. har ett formulär där användaren ska fylla i en e-postadress så kan du konvertera deras inmatning till små bokstäver för att undvika problem med skillnaden mellan stora och små bokstäver. Detta är särskilt viktigt när man ska göra jämförelser i databasfrågor eller när man utför en sökning i en lista eller array.

Ett annat tillfälle där man ofta använder sig av konvertering till små bokstäver är vid hantering av inloggning och lösenord. Genom att konvertera både användarnamn och lösenord till små bokstäver kan man säkerställa att inloggningen fungerar oavsett vilka bokstäver som användaren väljer att använda.

## Se även

- [JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [How to convert a string to lowercase in JavaScript](https://www.tutorialrepublic.com/faq/how-to-convert-a-string-to-lowercase-in-javascript.php)
- [Using JavaScript replace() method with a regular expression](https://www.freeformatter.com/javascript-regex-tester.html#ad-output-example)