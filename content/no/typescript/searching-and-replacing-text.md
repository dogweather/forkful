---
title:    "TypeScript: Søke og erstatte tekst"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave i mange programmeringsspråk, inkludert TypeScript. Det er en effektiv måte å endre tekst på en gang, i stedet for å gjøre det manuelt for hver forekomst.

## Hvordan

For å søke og erstatte tekst i TypeScript, kan du bruke den innebygde metoden "replace". Denne metoden tar to argumenter: den gamle teksten du vil endre, og den nye teksten du vil erstatte den med.

```TypeScript
let originalTekst = "Hei, verden!";
let nyTekst = originalTekst.replace("Hei", "Hallo");

console.log(nyTekst); // Output: Hallo, verden!
```

Her har vi laget en variabel "originalTekst" med en tekststreng "Hei, verden!" og brukt "replace" -metoden for å erstatte "Hei" med "Hallo". Den nye teksten "Hallo, verden!" blir lagret i variabelen "nyTekst" og deretter skrevet ut i konsollen.

Du kan også bruke regulære uttrykk (regex) til å gjøre mer avansert søk og erstatning. For eksempel, hvis du vil erstatte alle tall i en tekststreng med "#", kan du bruke følgende kode:

```TypeScript
let tallTekst = "1234567890";
let nyTekst = tallTekst.replace(/[0-9]/g, "#");

console.log(nyTekst); // Output: ##########
```

Her brukes en regex med karaktersettet "[0-9]" som matcher alle tall fra 0 til 9, og "g" -flagget som sier at den skal søke gjennom hele tekststrengen. Alle tall blir da erstattet med "#".

## Dypdykk

Som nevnt tidligere, kan du også bruke regex til mer avansert søk og erstatning. Dette inkluderer å bruke grupper og erstatningsvariabler.

For eksempel, hvis du har en tekststreng med e-postadresser og ønsker å bytte om domenenavnet, kan du gjøre det ved hjelp av følgende kode:

```TypeScript
let eposter = "bruker@eksempel.com, bruker2@eksempel.com";
let nyTekst = eposter.replace(/([a-z0-9._%+-]+)@eksempel.com/g, "$1@nytt-eksempel.com");

console.log(nyTekst); // Output: bruker@nytt-eksempel, bruker2@nytt-eksempel
```

Her bruker vi to grupper i regex - en for brukernavn og en for domenenavnet. Vi bruker deretter erstatningsvariabelen "$1" som refererer til den første gruppen (brukernavnet) og erstatter domenenavnet med "nytt-eksempel.com".

Det er mange forskjellige muligheter når det kommer til søk og erstatning med regex, og det er verdt å utforske dette dypere for å bli enda mer effektiv i din kode.

## Se også

- [TypeScript regex-dokumentasjon](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [JavaScript String replace-metoden](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)