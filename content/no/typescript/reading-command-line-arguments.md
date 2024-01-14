---
title:    "TypeScript: Lesing av kommandolinjeargumenter"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen bry seg om å lese kommandolinje-argumenter? Vel, svaret er enkelt - fordi det er en viktig del av å lage et robust og effektivt TypeScript-program. Når du har lært hvordan du kan lese og bruke disse argumentene, vil du kunne overraske brukerne dine med mer dynamiske og interaktive programmer.

## Hvordan

La oss se på et enkelt eksempel på hvordan du kan lese kommandolinje-argumenter i TypeScript:

```TypeScript
let args = process.argv.slice(2);
console.log(`Første argument: ${args[0]}`);
```

Hvis du nå kjører programmet ditt fra kommandolinjen med argumentet "hello", vil du få følgende utskrift:

```TypeScript
Første argument: hello
```

La oss nå ta en titt på hva som skjer i koden vår:

1. Vi bruker `process`-objektet til å få tilgang til informasjon om prosessen som kjører programmet vårt.

2. `process.argv` returnerer en liste over alle kommandolinje-argumentene som ble gitt da programmet ble kjørt.

3. Vi bruker `slice`-metoden for å fjerne de to første elementene i listen, siden disse vanligvis er banen til node-installasjonen og filbanen til programmet vårt. Dette gjør at vi kun sitter igjen med de argumentene som ble gitt av brukeren.

4. Til slutt bruker vi det første argumentet i listen for å skrive ut en beskjed i konsollen.

Dette er en veldig enkel måte å lese kommandolinje-argumenter på, og det er mange flere muligheter for å håndtere disse argumentene på en mer avansert måte.

## Deep Dive

For å virkelig utnytte potensialet til å lese kommandolinje-argumenter, er det viktig å forstå forskjellen mellom opsjoner og parametere. Opsjoner er vanligvis korte flagg som kan legges til argumentene for å aktivere bestemte funksjoner i programmet. Parametere derimot, er nøkkelverdipar som gir mer spesifikk informasjon til programmet.

Her er et eksempel på hvordan du kan implementere dette i koden din:

```TypeScript
let args = process.argv.slice(2);
let options = [];
let params = {};

for (let i = 0; i < args.length; i++) {
  if (args[i].startsWith("--")) {
    let option = args[i].substring(2);
    options.push(option);
  } else if (args[i].startsWith("-")) {
    let option = args[i].substring(1);
    options.push(option);
  } else {
    let [key, value] = args[i].split("=");
    params[key] = value;
  }
}

if (options.includes("help")) {
  console.log("Hjelpemeny: ...");
}

if (params["name"]) {
  console.log(`Hei, ${params["name"]}!`);
}
```

Dette er bare et eksempel på hvordan du kan håndtere de forskjellige typene kommandolinje-argumenter i programmet ditt. Det er mange flere strategier du kan bruke, avhengig av hva slags funksjonalitet du ønsker å implementere.

## Se også

- [Kommandolinjebruk i TypeScript-prosjekter](https://www.typescriptlang.org/docs/handbook/intro-to-js-ts.html#command-line-flags)
- [Node.js-prosessobjektet](https://nodejs.org/dist/latest-v14.x/docs/api/process.html)
- [Tydeligere kommandoer med strukturerte argumenter i TypeScript](https://auth0.com/blog/how-to-build-and-utilize-command-line-babel-with-node-js/)