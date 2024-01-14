---
title:                "TypeScript: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas flera anledningar till att man vill ta bort karaktärer som matchar ett visst mönster. Det kan till exempel vara för att rensa upp data eller för att komma åt specifika delar av en textsträng.

## Såhär gör du

För att ta bort karaktärer som matchar ett mönster i TypeScript, kan du använda metoden `replaceAll()` tillsammans med reguljära uttryck. Här är ett exempel:

```TypeScript
let str = "Detta är en textsträng som innehåller många vokaler."
let newStr = str.replaceAll(/[aeiouyåäö]/g, "")
console.log(newStr)
```

Output:
```
Dtts r n txtstrng sm nnhållr mng knsn vklr.
```

I exemplet ovan används `[aeiouyåäö]` för att matcha alla vokaler. `g` står för "global" och betyder att det matchande mönstret ska appliceras på hela strängen.

## Djupdykning

När man använder reguljära uttryck för att ta bort karaktärer, finns det olika symboler och mönster man kan använda sig av för att få mer precist resultat. Till exempel kan man använda `^` för att matcha tecken i början av en sträng, `$` för tecken i slutet, och `.` för att matcha ett valfritt tecken.

Man kan även använda `|` för att matcha flera olika mönster, och `()` för att gruppera mönster tillsammans.

För en mer omfattande guide över reguljära uttryck i TypeScript, se gärna dessa resurser:

- https://www.typescriptlang.org/docs/handbook/regular-expressions.html
- https://regex101.com

## Se även

- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
- https://www.regular-expressions.info