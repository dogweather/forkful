---
title:                "TypeScript: Generering av tilfeldige tall"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er et nyttig verktøy for mange programmer, enten det er for å lage spill, teste algoritmer eller lage unike brukernavn. Det er også ofte brukt i statistiske og matematiske beregninger.

## Hvordan gjøre det

For å generere tilfeldige tall i TypeScript, kan du bruke Math.random() funksjonen. Denne funksjonen returnerer et tilfeldig tall mellom 0 og 1.

```TypeScript
let nummer = Math.random();
console.log(nummer);
```

Dette vil gi et resultat som dette:

```
0.7314293287847
```

For å få et tall innenfor et bestemt område, kan du bruke følgende kode:

```TypeScript
let nummer = Math.floor(Math.random() * 100) + 1; // Dette vil gi et tall mellom 1 og 100
```

For å generere et tilfeldig desimaltall i et bestemt område, kan du bruke følgende kode:

```TypeScript
let nummer = (Math.random() * (max - min) + min).toFixed(decimaler); // Her må du endre "max", "min" og "decimaler" med ønskede verdier
```

Det er også mulig å generere tilfeldige bokstaver eller tegn ved å bruke charAt() funksjonen på en tekststreng og Math.floor() funksjonen for å få et tilfeldig tall.

## Dypdykk

Når du genererer tilfeldige tall, er det viktig å være klar over at de ikke er helt tilfeldige. De er basert på en "seed" verdi, som bestemmer den første verdien i en sekvens av tall som vil bli generert. Hvis du ønsker å få forskjellige tall hver gang du kjører koden, kan du bruke Date.now() funksjonen som en "seed" verdi.

Det er også viktig å bruke riktig tilfeldig tallgenerering for formålet ditt. For eksempel, hvis du lager et spill, bør du bruke en mer avansert tilfeldig tallgenereringsalgoritme for å unngå at det blir forutsigbart.

## Se også

- [Math.random() Dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Eksempler på tilfeldig tallgenerering i TypeScript](https://www.codementor.io/@innovationjs/random-number-generation-how-to-write-a-random-number-generator-in-node-js-tor1oa3hg)
- [Implementering av en tilfeldig tallgenereringsalgoritme i JavaScript](https://github.com/davidbau/seedrandom)