---
title:    "TypeScript: Att hitta längden på en sträng"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en vanlig uppgift inom programmering, oavsett om du jobbar med JavaScript, TypeScript eller något annat språk. Att förstå hur man går tillväga för att göra det kan hjälpa dig att skriva bättre och effektivare kod.

## Hur man gör det

För att hitta längden på en sträng i TypeScript kan du använda den inbyggda metoden "length". Detta fungerar på samma sätt som i JavaScript, och du kan använda det på vilken variabel eller sträng som helst. Här är ett exempel:

```TypeScript
let myString: string = "Hej, världen!";
console.log(myString.length);
```

Output: 13

Detta exempel visar hur man kan använda metoden "length" på en variabel som innehåller en sträng. Det är viktigt att komma ihåg att längden på en sträng inkluderar alla tecken, inklusive mellanslag.

Du kan också använda metoden "length" på en sträng direkt, utan att det finns någon variabel inblandad:

```TypeScript
console.log("Hej, världen!".length);
```

Output: 13

## Djupdykning

Det finns några viktiga saker att komma ihåg när man använder metoden "length" för att hitta längden på en sträng. För det första fungerar den endast på strängar, inte på andra datatyper som t.ex. nummer eller boolean-värden. Om du försöker använda metoden på en annan typ av variabel kommer du att få ett felmeddelande.

För det andra räknas alla tecken, inklusive mellanslag och specialtecken, som en del av längden. Detta innebär att om du har en sträng som innehåller ett mellanslag, t.ex. "Hej, världen!", kommer längden att vara 13, även om det bara finns 12 bokstäver.

## Se även

* [Officiell TypeScript dokumentation - Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
* [W3Schools - TypeScript String length](https://www.w3schools.com/jsref/jsref_length_string.asp)
* [MDN - String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)