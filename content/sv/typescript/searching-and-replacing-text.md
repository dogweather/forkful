---
title:    "TypeScript: Söka och ersätta text"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Det kan finnas många anledningar till varför man skulle vilja söka och ersätta text i ett program. Det kan vara för att uppdatera en gammal kod, korrigera stavfel eller bara för att göra en snabb ändring i en stor mängd text.

## Så här gör man
Det finns flera sätt att söka och ersätta text i en TypeScript-kod. Det enklaste sättet är att använda inbyggda metoder i TypeScript som `replace()` eller `replaceAll()`. Till exempel, om vi vill ersätta alla förekomster av ordet "hund" med "katt" i en sträng:

```TypeScript
let djur = "hundar är våra bästa vänner";

// ersätt alla "hund" med "katt"
let nyaDjur = djur.replace(/hund/g, "katt");

console.log(nyaDjur); // output: "kattar är våra bästa vänner"
```

Vi kan också använda regex (regular expressions) för att söka och ersätta text. Till exempel, om vi vill byta ut alla siffror i en sträng med en stjärna:

```TypeScript
let text = "Det finns 5 äpplen i fruktträdet";

// byt ut alla siffror med "*"
let nyText = text.replace(/\d/g, "*");

console.log(nyText); // output: "Det finns * äpplen i fruktträdet"
```

För mer avancerade sök- och ersättningsfunktioner, kan vi använda bibliotek som `regex-string-replace` eller `string-replace`.

## Djupdykning
Det är viktigt att förstå skillnaderna mellan `replace()` och `replaceAll()` metoder i TypeScript. Med `replace()` kommer endast den första förekomsten av söksträngen att ersättas. Medan `replaceAll()` ersätter alla förekomster av söksträngen.

Vi kan också använda flaggor i regex för att göra sökningen mer precisa. Till exempel, med flaggan `i` kommer sökningen att vara fall-insensitiv, vilket betyder att det inte spelar någon roll om söksträngen är skriven med stora eller små bokstäver.

En annan viktig aspekt av att söka och ersätta text är att vara medveten om att både söksträngen och ersättningssträngen kan vara reguljära uttryck. Detta ger oss möjlighet att göra mer komplexa och mångsidiga ersättningar.

## Se också
- [TypeScript dokumentation om replace()](https://www.typescriptlang.org/docs/handbook/global-objects.html#replace)
- [Regex i TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expression-handling.html)
- [regex-string-replace bibliotek](https://www.npmjs.com/package/regex-string-replace)
- [string-replace bibliotek](https://www.npmjs.com/package/string-replace)