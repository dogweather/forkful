---
title:    "TypeScript: Att Stora en Sträng"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att göra en sträng med stora bokstäver kan vara användbart för att förstärka eller markera en viss del av en sträng. Det kan också vara nödvändigt för att uppfylla specifika kodningsstandarder eller för att matcha sökningar som sker med hjälp av stora och små bokstäver.

## Hur man gör

```TypeScript
function capitalizeString(str: string): string {
  return str.toUpperCase();
}

console.log(capitalizeString("hej världen")); // HEJ VÄRLDEN
```

I exemplet ovan använder vi den inbyggda `toUpperCase()` metoden i JavaScript för att ändra alla bokstäver i en sträng till stora bokstäver. Vi kan också skriva en egen funktion för att göra detta, som i exemplet nedan:

```TypeScript
function capitalizeString(str: string): string {
  let result = "";

  for (let i = 0; i < str.length; i++) {
    if (str[i] >= "a" && str[i] <= "z") { // Kontrollerar om bokstaven är en liten bokstav
      result += String.fromCharCode(str.charCodeAt(i) - 32); // Omvandlar till motsvarande stor bokstav och lägger till i resultatet
    } else {
      result += str[i]; // Om det inte är en bokstav läggs den till som den är
    }
  }

  return result;
}

console.log(capitalizeString("hej världen")); // HEJ VÄRLDEN
```

Observera att den andra funktionen också kan användas i andra JavaScript-baserade språk, som till exempel vanlig JavaScript eller Java.

## Djupdykning

Att ändra en sträng till stora bokstäver involverar ofta UTF-8-kodningen av bokstäver och specialtecken. Vissa tecken kan ha fler än en UTF-8-byte och därför kan det vara nödvändigt att använda en speciell funktion som `toUpperCodePoint()` för att hantera dem på rätt sätt.

Det är också viktigt att förstå skillnaden mellan `toLowerCase()` och `toUpperCase()` i JavaScript, eftersom vissa bokstäver inte har en tydlig motsvarighet i ett annat huvudbokstavsläge. Till exempel har bokstaven "ẞ" (tyskt stort dubbel-s) ingen motsvarighet i små bokstäver och skulle därför behållas oförändrad om funktionen `toLowerCase()` används.

## Se även

- [JavaScript `toUpperCase()` funktion](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [UTF-8-teckenkodning](https://www.w3schools.com/charsets/ref_utf8.asp)
- [Capitalization Standards and Styles in Software Development](https://www.upwork.com/resources/coding-standards-styles/)