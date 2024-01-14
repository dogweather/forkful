---
title:    "TypeScript: Konvertere en streng til små bokstaver"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hva er hensikten med å konvertere en streng til små bokstaver?

Konvertering av en streng til små bokstaver er en viktig del av tekstbehandling og kan være nyttig for å behandle data og gjøre det enklere å søke og sammenligne tekst. Det kan også være nyttig i situasjoner der vi ønsker at all tekst skal være i samme format, for eksempel ved å formatere for bruk i en database eller ved å sammenligne brukerinntasting med forventet tekst.

## Slik gjør du det i TypeScript:

```TypeScript
let streng = "Dette er EN tekst"; //definerer en streng
streng = streng.toLocaleLowerCase(); //konverterer til små bokstaver
console.log(streng); //utskrift: dette er en tekst
```

```TypeScript
let navn = "Jørgen"; //definerer en streng
navn = navn.replace("ø", "o"); //erstatter én karakter med en annen
console.log(navn); //utskrift: Jorgen
```

## Gå i dybden

Når vi bruker metoden `toLocaleLowerCase()` på en streng i TypeScript, vil alle bokstavene bli konvertert til små bokstaver, i henhold til språkinnstillingene på enheten som kjører koden. Dette betyr at for noen språk, som norsk og tysk, vil noen bokstaver bli konvertert til en annen bokstav enn den tilsvarende engelske bokstaven. For eksempel vil "Å" bli konvertert til "å" i norsk, og "ß" til "ss" i tysk.

Det er viktig å merke seg at `toLocaleLowerCase()` ikke endrer strengen permanent, det vil si at den originale strengen forblir uendret. Hvis vi ønsker å lagre den konverterte strengen, må vi tilordne den til en ny variabel eller tilbake til den opprinnelige variabelen.

## Se også

- [MDN Web Docs: String.prototype.toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- [Blogginnlegg: Slik bruker du strenger i TypeScript](https://www.bayviewcoder.com/nb/slik-bruker-du-strenger-i-typescript/)