---
title:    "TypeScript: Konvertere en streng til små bokstaver"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å konvertere en streng til små bokstaver når man programmerer. Dette kan være nyttig når man for eksempel skal sammenligne to strenger uten å ta hensyn til store og små bokstaver, eller når man vil formatere en tekst på en bestemt måte.

## Slik gjør du det

For å konvertere en streng til små bokstaver i TypeScript, kan man bruke funksjonen `toLowerCase()`. Denne funksjonen konverterer alle bokstavene i en streng til små bokstaver. La oss se på et eksempel:

```TypeScript
let navn = "Jørgen";
console.log(navn.toLowerCase());
```

Dette vil gi følgende utskrift:

```TypeScript
jørgen
```

Vi kan også bruke `toLowerCase()` til å sammenligne to strenger uten å ta hensyn til store og små bokstaver:

```TypeScript
let passord = "Hemmelig123";
let brukerInput = "HEmMelig123";

if(passord.toLowerCase() === brukerInput.toLowerCase()) {
  console.log("Passordet er riktig!");
} else {
  console.log("Feil passord!");
}
```

Dette vil gi følgende utskrift:

```TypeScript
Passordet er riktig!
```

## Dypdykk

Når man bruker `toLowerCase()` funksjonen, konverteres kun bokstavene til små bokstaver. Eventuelle tall, symboler eller mellomrom i strengen forblir uendret. Det er også viktig å merke seg at `toLowerCase()` funksjonen ikke endrer selve variabelen, men heller returnerer en ny streng med de konverterte bokstavene.

## Se også

- [MDN web docs - toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript dokumentasjon - String Operations](https://www.typescriptlang.org/docs/handbook/strings.html)