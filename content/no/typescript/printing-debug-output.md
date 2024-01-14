---
title:    "TypeScript: Utskrift av feilsøkingsutdata"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive kodedebuggeringsutdata er en essensiell del av utviklingsprosessen. Det hjelper deg med å finne og løse feil i koden din og gjør debugging-prosessen enklere og mer effektiv. Uten å ha tilgang til utdata, kan feilene være vanskelige å identifisere og rette.

## Slik gjør du det

For å skrive debug output i TypeScript, kan du bruke funksjonen `console.log()`. Dette vil skrive ut innholdet du ønsker å sjekke i konsollen din. La oss se på et enkelt eksempel:

```TypeScript
let navn = "Lars";
console.log("Hei " + navn);
```

Når du kjører denne koden, vil du se følgende utdata i konsollen din:

```TypeScript
Hei Lars
```

Du kan også skrive ut verdier av variabler og objekter ved å inkludere dem som en del av utdataen din. La oss se på et annet eksempel:

```TypeScript
let alder = 25;
let person = {
  navn: "Kari",
  yrke: "Programmerer"
};
console.log(person.navn + " er " + alder + " år gammel og er en dyktig " + person.yrke + ".");
```

Dette vil skrive ut følgende:

```TypeScript
Kari er 25 år gammel og er en dyktig Programmerer.
```

## Dypdykk

Å skrive debug output i TypeScript kan også være nyttig for å forstå hvordan koden din fungerer og finne ineffektive deler av koden. Ved å legge til utdata i ulike deler av koden din, kan du se hvordan verdiene endrer seg og hvordan programmet ditt oppfører seg i ulike situasjoner.

Det er også mulig å endre hvordan konsollen din viser utdata ved å bruke forskjellige formateringsalternativer som `%d` for numeriske verdier og `%s` for tekst. Du kan lese mer om disse alternativene i dokumentasjonen til TypeScript.

## Se også

- [Console API](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Debugging in TypeScript](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [Using console.log() in TypeScript](https://www.digitalocean.com/community/tutorials/typescript-console-log-language)