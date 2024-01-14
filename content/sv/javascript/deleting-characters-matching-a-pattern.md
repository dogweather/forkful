---
title:    "Javascript: Radera tecken som matchar ett mönster"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster är en vanlig uppgift inom programmering som kan ha olika användningsområden. Det kan användas för att rena data, ersätta ogiltiga tecken eller för att förbättra prestandan hos en applikation.

## Hur man gör

För att ta bort tecken som matchar ett specifikt mönster i JavaScript kan vi använda metoden `replace()` tillsammans med en reguljär uttryck (regEx). Nedan följer ett exempel på kod som tar bort siffrorna från en sträng:

```Javascript
let str = "123abc";
str = str.replace(/[0-9]/g, '');
console.log(str); // utmatning: abc
```

Vi använder `/[0-9]/g` som vårt regEx-mönster för att identifiera alla siffror (`0-9`) i strängen och `g` för att tala om för metoden att vi vill ersätta alla förekomster, inte bara den första.

Vi kan också använda en annan metod, `split()`, för att ta bort tecken som matchar ett mönster. Denna metod delar upp strängen i delar baserat på mönstret och returnerar en array av delarna. Därefter kan vi enkelt sammanfoga dessa delar med hjälp av `join()`-metoden. Exempel:

```Javascript
let str = "abc123";
str = str.split(/[0-9]/).join('');
console.log(str); // utmatning: abc
```

## Fördjupning

Reguljära uttryck är en kraftfull funktion inom programmering som låter oss söka, matcha och manipulera text på ett effektivt sätt. De består av ett mönster och flaggor som specificerar hur sökningen ska utföras. RegEx stöds i många programmeringsspråk, inklusive JavaScript.

Det finns många användbara verktyg och resurser online för att lära sig mer om reguljära uttryck och hur man använder dem i JavaScript, som [RegExr](https://regexr.com/) och [MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions).

## Se också

- [Introduction to Regular Expressions in JavaScript](https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions-in-javascript)
- [Mastering JavaScript Regular Expressions](https://www.pluralsight.com/blog/software-development/mastering-regular-expressions-in-javascript)