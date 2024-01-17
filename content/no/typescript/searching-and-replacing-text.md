---
title:                "Søke og erstatte tekst"
html_title:           "TypeScript: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Søking og erstattelse av tekst er en vanlig oppgave i programmering. Det refererer til å finne et bestemt mønster eller streng i en tekst og erstatte det med en annen. Dette kan være nyttig for å gjøre store endringer i en kodebase eller for å finne og fikse feil.

Programmere gjør dette for å effektivt håndtere kode med store tekstblokker og for å automatisere å gjøre endringer i kode.

## Hvordan:

```TypeScript
// Eksempel på søking og erstatting ved hjelp av JavaScripts replace() metode
let tekst = "Hei, mitt navn er Jane";
let nyTekst = tekst.replace("Jane", "John");
console.log(nyTekst);
// Output: "Hei, mitt navn er John"
```

```TypeScript
// Eksempel på å bruke regulære uttrykk (regular expressions) for å søke og erstatte tekst
let tekst = "Årstidene er høst, vinter, vår og sommer";
let nytekst = tekst.replace(/høst/g, "autumn").replace(/vinter/g, "winter").replace(/vår/g, "spring").replace(/sommer/g, "summer");
console.log(nyTekst);
// Output: "Årstidene er autumn, winter, spring og summer"
```

## Dive Deep:

Søking og erstattelse av tekst har vært en viktig del av programmering siden de tidlige dagene. I motsetning til dagens teknologi hvor det er enkelt å søke og erstatte tekst med få tastetrykk, var det tidligere en mer utfordrende og tidkrevende oppgave. Det finnes også alternative metoder for å søke og erstatte tekst, som å lage en egen funksjon som itererer gjennom teksten og utfører erstatning for hvert funnet mønster.

I TypeScript kan man bruke både JavaScripts replace() metode og regulære uttrykk for å søke og erstatte tekst. Metoden replace() tar imot to argumenter, en streng med det opprinnelige mønsteret som skal erstattes og en streng med hva det skal erstattes med. Regulære uttrykk gir derimot muligheten for å søke etter mer komplekse mønstre og også å gjøre endringer basert på disse mønstrene.

## Se også:

- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/nb/docs/Web/JavaScript/Guide/Regular_Expressions)