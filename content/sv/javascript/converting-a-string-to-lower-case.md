---
title:    "Javascript: Omvandla en sträng till gemener"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Är du nybörjare inom programmering och undrar varför det är viktigt att kunna konvertera en sträng till gemena bokstäver? Eller är du en erfaren programmerare som behöver repetera? Oavsett din nivå, så är det viktigt att förstå varför detta är en viktig funktion inom Javascript.

## Hur man gör
Konvertering av en sträng till gemena bokstäver är enkelt med hjälp av Javascripts inbyggda funktion "toLowerCase()". Se kodexemplen nedan för att se hur det fungerar och vilken output som förväntas.

```Javascript
// 1. Gör strängen "Hej!" till gemena bokstäver
let sträng = "Hej!";
let gemenaBokstäver = sträng.toLowerCase();

console.log(gemenaBokstäver); // output: "hej!"

// 2. Konvertera en användares input till gemena bokstäver
let användarInput = prompt("Skriv en text:");
let gemenaBokstäver = användarInput.toLowerCase();

console.log(gemenaBokstäver); // input: "JavaScript", output: "javascript"
```

Som du kan se i exemplen ovan så är det mycket enkelt att konvertera en sträng till gemena bokstäver. Genom att använda "toLowerCase()" funktionen så kommer du alltid att få en sträng med alla gemena bokstäver.

## Djupdykning
Förutom att konvertera en sträng till gemena bokstäver så finns det andra användbara metoder för strängmanipulering i Javascript. Till exempel "toUpperCase()" som gör om alla bokstäver till versala bokstäver och "replace()" som byter ut en viss del av en sträng med en annan del. Det är viktigt att ha koll på olika metoder för strängmanipulering för att kunna skapa funktionella och effektiva program.

## Se även
- [W3Schools - Javascript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [MDN Web Docs - String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase) 
- [MDN Web Docs - String Manipulation](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/First_steps/Strings)