---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "TypeScript: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utskrift av feilsøkingsutdata er en metode som utviklere bruker for å identifisere og løse feil i sitt programvareprosjekt. Det er nyttig for å finne hvor en feil oppstår og hva som skjer i koden når det skjer.

## Slik gjør du:
```TypeScript
// Opprett en funksjon for å logge ut en melding
function loggMelding(melding: string) {
    console.log("Melding:", melding);
}
// Kall funksjonen med din melding
loggMelding("Hei fra TypeScript!");
```
Dette vil skrive ut "Melding: Hei fra TypeScript!" i konsollen din.

## Dykk dypere:
Feilsøkingsutdata utskrift har vært en langvarig praksis i utvikling av programvare. Alternativene til utskrift av feilsøkingsutdata inkluderer bruk av eksterne verktøy for feilsøking og logging, men disse kan være mer komplisert og tidkrevende. Implementasjonen av feilsøkingsutdata utskrift er enkelt i TypeScript og lar utviklere enkelt logge ut variabler og meldinger til konsollen for å finne feil.

## Se også:
- [TypeScript offisiell dokumentasjon](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html)
- [Alternativer til utskrift av feilsøkingsutdata](https://www.codementor.io/blog/avoid-logging-spam-in-node-and-the-browser-5hkqk8sntq)
- [Diskusjonsfora for TypeScript programmering](https://www.reddit.com/r/typescript/)