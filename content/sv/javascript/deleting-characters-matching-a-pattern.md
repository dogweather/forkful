---
title:                "Javascript: Radera tecken som matchar ett mönster."
simple_title:         "Radera tecken som matchar ett mönster."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Att ta bort tecken som matchar ett mönster kan vara användbart i många olika situationer inom programmering. Det kan till exempel vara för att rensa oönskade tecken från en textsträng eller för att skapa en mer strukturerad och läsbar kod.

## Hur man gör
För att ta bort tecken som matchar ett specifikt mönster i Javascript, kan vi använda metoden `replace` på en sträng. Syntaxen för denna metod är `str.replace(regexp, replacer)`, där `str` är vår ursprungliga sträng, `regexp` är det mönster vi vill matcha och `replacer` är det tecken eller den sträng vi vill ersätta matchningen med.

```Javascript
let originalStr = "Välkommen till mitt blogginlägg! #javascript #kodning";
let newStr = originalStr.replace(/[#]/g, "");
console.log(newStr); // Välkommen till mitt blogginlägg! javascript kodning
```

I exemplet ovan använder vi `replace` för att ta bort alla förekomster av tecknet `#` från vår ursprungliga sträng. Genom att använda flera metakaraktärer inom vårt mönster, t.ex. `/[.?]/g`, kan vi även ta bort andra tecken som förekommer i vår sträng. Detta gör det möjligt att effektivt rensa bort oönskade tecken från en textsträng.

## Djupdykning
Metoden `replace` accepterar inte bara strängar som ersättning, utan även en callback-funktion. Detta gör det möjligt att göra en djupare behandling av de matchande tecknen. Callback-funktionen tar emot flera parametrar, inklusive en matchande del av strängen och dess position i strängen. Detta kan vara användbart för mer avancerade behandlingar av textsträngar.

En annan användbar funktion för att ta bort tecken som matchar ett mönster är `split`, som delar upp en sträng i en array av mindre strängar baserat på ett angivet mönster. Genom att kombinera `split` med `join` kan vi enkelt ta bort oönskade tecken från en sträng.

```Javascript
let originalStr = "Välkommen till mitt blogginlägg! #javascript #kodning";
let newStr = originalStr.split(/[#]/).join("");
console.log(newStr); // Välkommen till mitt blogginlägg! javascript kodning
```

## Se även
- [MDN: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN: String.prototype.split()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN: String.prototype.join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)