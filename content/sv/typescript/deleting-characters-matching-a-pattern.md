---
title:                "Radera tecken som matchar ett mönster"
html_title:           "TypeScript: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster är en användbar funktion i programmering för att rensa data eller hantera strängar på ett effektivt sätt. Det kan också användas för att filtrera ut oönskad information eller skapa användarvänliga formulär med automatisk strängvalidering.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster i en sträng, kan vi använda funktionen `replace()` i TypeScript. Syntaxen ser ut som följande:

```TypeScript
let sträng = "Exempelsträng"
sträng = sträng.replace(/mönster/g, "");
console.log(sträng); // Utmatning: Exempel
```

I detta exempel ersätter vi alla tecken som matchar mönstret "/mönster/" (här representerat av ordet "sträng") med en tom sträng, vilket i princip tar bort dessa tecken från den ursprungliga strängen.

Vi kan också använda en reguljär uttrycksspaning som mönster, till exempel "/[A-Za-zåäöÅÄÖ]+/" för att ta bort alla bokstäver från en sträng. Här är ett annat exempel:

```TypeScript
let sträng = "123ABC789"
sträng = sträng.replace(/[A-Za-zåäöÅÄÖ]+/g, "");
console.log(sträng); // Utmatning: 123789
```

Vi kan använda alla reguljära uttrycksmönster som stöds av TypeScript för att ta bort specifika tecken eller teckengrupper från en sträng.

## Djupdykning

I TypeScript kan vi också använda funktionen `match()` för att hitta och extrahera alla delar av en sträng som matchar ett givet mönster. Denna funktion tar ett reguljärt uttryck som argument och returnerar en array med alla matchande delar av strängen.

```TypeScript
let sträng = "Detta är ett exempelsträng"
let matcher = sträng.match(/[a-zåäö]+/g);
console.log(matcher); // Utmatning: ["etta","är","ett","exempelsträng"]
```

Här hittar vi alla små bokstäver i strängen och extraherar dem till en array för ytterligare behandling.

## Se också

- [TypeScript dokumentation för replace()] (https://www.typescriptlang.org/docs/handbook/declaration-merging.html)
- [GitHub-repository för reguljära uttryck i TypeScript] (https://github.com/Microsoft/TypeScript/pull/17546)
- [Enkel guide till reguljära uttryck i TypeScript] (https://www.digitalocean.com/community/tutorials/js-regex-regular-expressions-modifier-flags)