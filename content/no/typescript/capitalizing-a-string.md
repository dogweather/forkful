---
title:    "TypeScript: Å få store bokstaver i en streng"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor
En av de grunnleggende funksjonene i mange programmeringsspråk er muligheten til å endre strenger til å være enten helt store eller helt små bokstaver. Dette kan være nyttig for å lage tydeligere og mer konsistent tekstutgang, samt sortere data på en mer logisk måte.

## Hvordan
For å muliggjøre denne funksjonen i TypeScript, kan du bruke to forskjellige metoder: enten ved hjelp av string metoder, eller ved å bruke en egen funksjon som kan akseptere en streng som et argument.

### String metoder
String metoder er innebygde funksjoner i TypeScript som kan brukes til å manipulere strenger. En av disse metodene er `.toUpperCase()`, som konverterer alle bokstaver i en streng til store bokstaver.

```TypeScript
let tekst = "dette er en test";
console.log(tekst.toUpperCase());

// Output: "DETTE ER EN TEST"
```

### Egen funksjon
Hvis du ønsker å ha mer kontroll over hvordan strengen blir kapitalisert, kan du lage din egen funksjon som tar inn en streng som et argument og returnerer den med store bokstaver.

```TypeScript
function kapitaliserStreng(streng: string) {
  let kapitalisertStreng = "";
  for (let i = 0; i < streng.length; i++) {
    let bokstav = streng[i];
    if (bokstav >= "a" && bokstav <= "z") {
      kapitalisertStreng += bokstav.toUpperCase();
    } else {
      kapitalisertStreng += bokstav;
    }
  }
  return kapitalisertStreng;
}

console.log(kapitaliserStreng("dette er en test"));

// Output: "DETTE ER EN TEST"
```

## Dypdykk
Å kapitalisere strenger kan virke som en enkel oppgave, men det er viktig å merke seg at det kan være ulike regler for store bokstaver i forskjellige språk. Noen språk har for eksempel bokstaver som består av flere deler, og det kan være viktig å håndtere disse riktig når du kapitaliserer en streng.

En annen ting å ha i bakhodet er at noen språk, som for eksempel tysk, har egne regler for store bokstaver når de kommer midt i en setning. Dette kan være en utfordring når du arbeider med internasjonalisering av programmer.

## Se også
- [Dokumentasjon for string metoder i TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-5.html#string-handling-additions)
- [Eksempler på internasjonaliseringsutfordringer i programmering](https://www.sitepoint.com/the-trouble-with-internationalization/)