---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och varför?
Radering av karaktärer som matchar ett mönster i JavaScript innebär att vi tar bort vissa karaktärer ur en sträng som matchar ett specifikt förebild. Det är nödvändigt för programmerare att göra detta för att manipulera data, tex. rensa data från oönskade tecken eller för att förbereda data för vidare bearbetning.

## Hur man gör:
Här ska vi använda `replace()`-metoden i JavaScript med ett reguljärt uttryck. Kika på följande exempel:

```Javascript
let str = "Hej, string med mycket skräp! ###!!!.";
let nyStr = str.replace(/[^a-zA-Z ]/g, "");
console.log(nyStr);
```

Output:
```Javascript
"Hej string med mycket skräp"
```

I koden ovan skapar vi en sträng `str`, sedan använder vi `replace()`-metoden för att ta bort alla tecken som inte är bokstäver. `[^a-zA-Z ]` betyder "inte en bokstav".

## Fördjupning
Ursprungligen, innan ES6, utfördes tillämpningen av att ta bort karaktärer med hjälp av slingor och konditionaler som ofta ledde till klumpiga lösningar. Med införandet av reguljära uttryck och deras integrering i JavaScript blev detta mycket smidigare. 

Det finns alternativ till 'replace()' metoden, såsom användning av 'filter()' metoden eller att konvertera strängen till ett array och sedan tillbaka till en sträng.

Det är viktigt att notera att metoden `replace()` inte modifierar originalet utan returnerar en ny sträng, vilket betyder att om vi vill behålla de ändringar vi gör måste vi tilldela resultatet till en ny variabel eller till samma variabel igen.

## Se vidare
För mer information om att arbeta med reguljära uttryck i JavaScript, kolla in följande länkar:

- [Reguljära uttryck (Regular Expressions) på MDN Web Docs](https://developer.mozilla.org/sv-SE/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Rensa strängar med reguljära uttryck](https://www.freecodecamp.org/news/javascript-regex-expressions/)
- [replace()-metoden på MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)