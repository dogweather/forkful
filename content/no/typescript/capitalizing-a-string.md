---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
En tekststreng blir stor bokstav når hvert ord starter med en stor bokstav. Dette er nyttig for å formatere tekst slik at titler, navn eller overskrifter ser korrekte og profesjonelle ut.

## How to:
For å gjøre om en string til stor bokstav i TypeScript, kan du bruke `toLowerCase()` og `replace()` metodene, eller lage din egen funksjon. Her er et eksempel:

```typescript
function capitalizeString(input: string): string {
  return input.toLowerCase().replace(/\b\w/g, letter => letter.toUpperCase());
}

// Brukseksempel
const title = "hallo, dette er et eksempel.";
const capitalizedTitle = capitalizeString(title);

console.log(capitalizedTitle);  // Output: "Hallo, Dette Er Et Eksempel."
```

## Deep Dive
I tidligere epoker av programmering, var det ikke uvanlig å håndtere bokstaver og tekst manuelt. Dette, som mye annet, har blitt lettere med moderne programmeringsspråk og deres innebygde stringfunksjoner. Alternativer til `replace()` kunne være å bruke biblioteker som lodash sine `_.startCase()`.

Når det gjelder implementasjonsdetaljer, tar `replace()`-metoden i eksempelet over en regex som første argument som finner alle ordgrenser etterfulgt av et alfanumerisk tegn. Den andre parameteren er en funksjon som gjør det individuelle bokstavet stor bokstav.

En annen ting å vurdere er lokaliseringsbehov – noen språk har bokstaver som ikke er dekket av standard `toUpperCase()`-metoden.

## See Also
- MDN Web Docs for String operations: [String - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Lodash biblioteket for diverse stringmanipulasjoner: [Lodash](https://lodash.com/docs/4.17.15#startCase)
- TypeScript offisielle dokumentasjon for mer avansert type manipulation: [TypeScript Documentation](https://www.typescriptlang.org/docs/)
