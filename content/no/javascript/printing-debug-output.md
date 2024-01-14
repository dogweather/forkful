---
title:                "Javascript: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å printe ut feilsøkingsutdata kan være en nyttig praksis for å feilsøke og finne feil i koden din. Det kan hjelpe deg med å forstå hva som skjer under kjøringen av programmet ditt og finne ut hvor det kan være problemer.

## Hvordan

For å printe ut feilsøkingsutdata i Javascript, kan du bruke `console.log()` funksjonen. Denne funksjonen vil skrive ut gitt data til utgangskonsollen i nettleseren din.

```Javascript
let navn = "Sarah";
let alder = 25;
let yrke = "Webutvikler";
console.log("Navn: " + navn);
console.log("Alder: " + alder);
console.log("Yrke: " + yrke);
```

Dette vil resultere i følgende utdata i konsollen:

```
Navn: Sarah
Alder: 25
Yrke: Webutvikler
```

I tillegg til å skrive ut variabler, kan du også printe ut tekster eller til og med objekter:

```Javascript
console.log("Feilmelding: En ugyldig input ble oppdaget.");
console.log({ navn: "John", alder: 28, yrke: "Designer" });
```

Dette vil resultere i:

```
Feilmelding: En ugyldig input ble oppdaget.
{ navn: "John", alder: 28, yrke: "Designer" }
```

## Dypdykk

Ved å bruke `console.log()` funksjonen, kan du også formatere utdataen din på en mer nyttig måte. For eksempel kan du bruke `%c` for å legge til farger og stil i teksten din:

```Javascript
console.log("%cFeilmelding: En ugyldig input ble oppdaget.", "color: red; font-weight: bold");
```

Dette vil gi deg en mer iøynefallende feilmelding i konsollen din.

En annen nyttig funksjon er `console.table()`, som lar deg skrive ut data i en tabellform:

```Javascript
let brukere = [
    { navn: "Lisa", alder: 32, yrke: "Lærer" },
    { navn: "David", alder: 29, yrke: "Selger" },
    { navn: "Maria", alder: 35, yrke: "Sykepleier" }
];
console.table(brukere);
```

Dette vil gi deg følgende utdata i konsollen din:

```
┌─────────┬───────┬─────────────┐
│ (index) │ navn  │    Verdi    │
├─────────┼───────┼─────────────┤
│    0    │ 'Lisa'│ [object ...]│
│    1    │ 'David' │ [object ...]│
│    2    │ 'Maria' │ [object ...]│
└─────────┴───────┴─────────────┘
```

Ved hjelp av disse funksjonene kan du enkelt få en bedre forståelse av hva som skjer under kjøringen av koden din og finne eventuelle feil eller problemer.

## Se også

- [Javascript konsole API-dokumentasjon](https://developer.mozilla.org/nb/docs/Web/API/Console)
- [5 tips for effektiv feilsøking i Javascript](https://medium.com/swlh/5-tips-to-efficiently-debug-your-javascript-code-a11447b057dd)
- [Debugging i Javascript](https://www.digitalocean.com/community/tutorials/how-to-debug-node-js-with-the-built-in-debugger-and-chrome-devtools)