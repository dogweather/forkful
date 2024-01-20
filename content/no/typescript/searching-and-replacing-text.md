---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# "## Hva & Hvorfor?"

Søking og erstatning av tekst betyr å finne en bestemt streng av tegn i et større skript og erstatte den med en annen streng. Programmører gjør dette for å automatisere feilretting, oppdatere data eller manipulere tekst.

# "## Hvordan:"

Å søke og erstatte tekst i TypeScript kan oppnås med bruk av `.replace()` funksjonen. Her er et eksempel:

```TypeScript
let tekst = "Hei, verden!";
tekst = tekst.replace("verden", "Norge");
console.log(tekst);
```

Utskriften vil være: "Hei, Norge!".

Du kan også bruke et RegExp objekt til å matche mønstre. For eksempel, for å erstatte alle tilfeller av "verden":

```TypeScript
let tekst = "Hei, verden! verden er vakker.";
tekst = tekst.replace(/verden/g, "Norge");
console.log(tekst);
```

Utskrift: "Hei, Norge! Norge er vakker."

# "## Dyp Dykking:"

Søk og erstatning av tekst har vært i bruk så lenge vi har datahåndteringssystemer. I TypeScript er `.replace()` en del av String API, arvet fra JavaScript.

Det finnes alternative måter å søke og erstatte tekst på i TypeScript, som split og join, men `.replace()` er ofte mer lesbar og effektiv.

```TypeScript
let tekst = "Hei, verden! verden er vakker.";
tekst = tekst.split("verden").join("Norge");
console.log(tekst);
```

Utskrift: "Hei, Norge! Norge er vakker."

Under metoden `.replace()`, søkemekanismen går gjennom hvert tegn i strengen en etter en til den finner et match. Denne lineære søkeprosessen gjør at `.replace()` har en tidskompleksitet på O(n).

# "## Se Også:"

- [Mozilla Developer Network - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)