---
title:                "Å arbeide med yaml"
html_title:           "Javascript: Å arbeide med yaml"
simple_title:         "Å arbeide med yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skal du bry deg om YAML? Det er en god måte å strukturere og organisere data på, spesielt for utviklere som jobber med store og komplekse prosjekter.

## Hvordan

YAML, eller "YAML Ain't Markup Language", er en måte å representere data på i lesbar og strukturert form. Det kan brukes til å konfigurere applikasjoner, definere datastrukturer og mye mer. La oss se på noen eksempler på hvordan man kan bruke YAML i Javascript.

```Javascript
// Opprette et enkelt YAML-dokument
let person = `
navn: John Doe
alder: 30
`

// Konvertere YAML til et Javascript-objekt
let personObj = YAML.parse(person);
console.log(personObj.navn); // Output: John Doe

// Opprette et YAML-dokument med en liste
let katter = `
- navn: Luna
  rase: Siamesisk
- navn: Milo
  rase: Bengal
`

// Konvertere YAML til et Javascript-objekt
let katterArr = YAML.parse(katter);
console.log(katterArr[1].rase); // Output: Bengal
```

Det er også mulig å generere YAML-filer ved hjelp av Javascript. La oss si at vi ønsker å opprette en konfigurasjonsfil for en app som heter "ToDo". Her er et eksempel på hvordan det kan gjøres:

```Javascript
// Opprette et objekt med dataen vi vil konvertere til YAML
let appKonfig = {
  navn: "ToDo",
  farger: {
    primær: "grønn",
    sekundær: "blå"
  },
  ikon: "checkmark",
  oppgaver: ["Handle mat", "Sende e-post", "Betale regninger"]
};

// Konvertere dataen til YAML og skrive til fil
let yamlKonfig = YAML.stringify(appKonfig);
fs.writeFileSync('todo.yml', yamlKonfig, 'utf8');
```

## Deep Dive

Nå som vi har sett på hvordan man kan bruke YAML i Javascript, la oss ta en dypere titt på noen av de viktigste konseptene rundt YAML.

### Datastrukturer

Som vi har sett i eksemplene ovenfor, kan YAML brukes til å representere forskjellige typer datastrukturer, som objekter, lister og nøster av disse. Dette gjør det enkelt å organisere og strukturere data på en lesbar måte.

### Indentering

En av de viktigste aspektene ved YAML er indentering. Dette er hvordan YAML-bestanden er strukturert med innrykk for å vise relasjoner mellom ulike strukturer. Det er viktig å merke seg at indentering må være konsekvent for at YAML skal fungere som forventet.

### Kommentarer

YAML støtter også kommentarer, som er veldig nyttig for å forklare hva hver del av filen gjør og hvorfor det er der. Kommentarer kan legges til ved å bruke "#" tegnet.

### Variabler og referanser

YAML støtter også variabler og referanser, noe som gjør det enkelt å gjenbruke data i en YAML-fil. Dette er spesielt nyttig når man jobber med store og komplekse YAML-strukturer.

## Se også

* Offisiell YAML-dokumentasjon: https://yaml.org/
* YAML-lint (verktøy for å sjekke og validere YAML-kode): https://yamllint.readthedocs.io/
* YAML i 5 minutter (en rask innføring i YAML): https://learnxinyminutes.com/docs/no-no/yaml-no/