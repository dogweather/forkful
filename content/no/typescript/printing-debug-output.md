---
title:                "Utskrift av feilsøkingsdata"
html_title:           "TypeScript: Utskrift av feilsøkingsdata"
simple_title:         "Utskrift av feilsøkingsdata"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor

Utskrift av feilsøkingsutdata er en viktig del av utviklingsprosessen for å finne og løse feil i koden. Det tillater utviklere å se hva som skjer i koden på et gitt tidspunkt, og kan bidra til å finne potensielle problemer som ellers ville være vanskelig å oppdage.

# Hvordan gjøre det

Det er flere måter å skrive ut feilsøkingsutdata i TypeScript, avhengig av hva du prøver å oppnå og hva som passer best for ditt prosjekt. Her er noen eksempler på hvordan du kan printe ut feilsøkingsutdata i TypeScript:

```TypeScript
// Skriv ut en enkel melding
console.log("Hei, dette er en feilsøkingsmelding");

// Skriv ut verdien av en variabel
let num = 10;
console.log("Verdien av num er: " + num);

// Skriv ut verdien av et objekt
let person = { navn: "Per", alder: 20 };
console.log("Navnet og alderen til personen er: " + person.navn + ", " + person.alder);
```

Dette vil resultere i følgende output:

```bash
Hei, dette er en feilsøkingsmelding
Verdien av num er: 10
Navnet og alderen til personen er: Per, 20
```

# Dypdykk

For å få mer detaljert informasjon om hva som skjer i koden din, kan du også bruke `debugger`-kommandoen i TypeScript. Dette vil bryte koden din på et bestemt punkt og tillate deg å undersøke verdier og status for ulike variabler på det aktuelle tidspunktet. Du kan bruke `debugger` på flere måter, for eksempel ved å plassere den i koden din som en breakpoint, eller ved å kjøre koden din med `--inspect` flagget.

Det finnes også forskjellige verktøy og biblioteker som kan hjelpe deg med å feilsøke i TypeScript-koden din, som for eksempel `console.time()` og `console.timeEnd()` for å måle hvor lang tid en operasjon tar, eller `console.assert()` for å sjekke om en gitt betingelse er sann og skrive ut en feilmelding hvis den ikke er det.

# Se også

- [https://www.typescriptlang.org/docs/handbook/introduction.html](https://www.typescriptlang.org/docs/handbook/introduction.html)
- [https://blog.risingstack.com/node-js-debugging-guide/](https://blog.risingstack.com/node-js-debugging-guide/)
- [https://developer.mozilla.org/en-US/docs/Web/API/console](https://developer.mozilla.org/en-US/docs/Web/API/console)