---
title:                "Söka och ersätta text"
html_title:           "Bash: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad och varför?

Sökning och ersättning av text innebär att man lokaliserar specifika strängar i text och byter ut dem mot någonting annat, vilket är en vanlig uppgift för programmerare. Den används för allt, från att uppdatera variabelnamn till att modifiera datavärden.

## Hur gör man:

Här är några grundläggande exempel på att söka och byta strängar i TypeScript:

```TypeScript
let text: string = 'Hej världen!';
let sök: string = 'världen';
let ersätt: string = 'Sverige';
let nyText: string = text.replace(sök, ersätt);

console.log(nyText);
// Output: 'Hej Sverige!'
```

Notera att `replace`-metoden endast ersätter den första förekomsten av 'sök'-strängen. För att ersätta alla förekomster, använd en RegExp med flaggan 'g', på följande sätt:

```TypeScript
let text: string = 'Hej världen, världen!';
let nyText: string = text.replace(/världen/g, 'Sverige');
console.log(nyText);
// Output: 'Hej Sverige, Sverige!'
```

## Fördjupning:

Sökning och ersättning av text har varit en grundläggande funktion i programmering sedan starten. Det var nödvändigt för att uppdatera, korrigera och optimera kod. 

Alternativ till `replace`-metoden inkluderar uttrycklig looping och indexering, men dessa metoder kan ofta vara mer komplexa och tidskrävande att implementera.

Viktig information om implementeringen av sökning och ersättning av text i TypeScript: `replace`-metoden är case-sensitive. Om du behöver en case-insensitive sökning, använd RegExp med 'i'-flaggan.

```TypeScript
let text: string = 'Hej VÄRLDEN!';
let nyText: string = text.replace(/världen/i, 'Sverige');
console.log(nyText);
// Output: 'Hej Sverige!'
```

## Se också:

- [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) - Innehåller mer detaljerad information om replace-funktionen, flaggor och dess användning.
- [w3schools](https://www.w3schools.com/jsref/jsref_replace.asp) - Enkel och tydlig guide om replace-funktionen och dess användningsområden.