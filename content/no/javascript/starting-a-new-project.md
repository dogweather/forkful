---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starte et nytt Javascript-prosjekt: en enkel guide

## Hva & Hvorfor?
Et nytt prosjekt er en ny start, et tomt lerret hvor du kan bygge noe fantastisk fra bunnen av. Programmerere starter nye prosjekter for å lære noe nytt, skape et nytt produkt, eller bare for å ha det gøy.

## Hvordan:
Opprett først en ny mappe for prosjektet ditt, så gå inn i den mappen. Bruk terminalen din til å utføre disse kommandoene:

```Javascript
mkdir myNewProject
cd myNewProject
```
Vi tar i bruk "npm init" for å opprette en ny 'package.json' fil:
```Javascript
npm init -y
```
Dette vil opprette en 'package.json' fil som holder styr på prosjektets metadata og avhengigheter. Outputen vil se slik ut:

```Javascript
{
  "name": "myNewProject",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "keywords": [],
  "author": "",
  "license": "ISC"
}
```
Du er nå klar til å legge til kodefilene dine og starte prosjektet!

## Dyp Dykk
Historisk sett har hver ny Versjon JavaScript brakt forbedringer som gjør prosjektstart enklere og mer effektiv. For eksempel, før pakkebehandlere som NPM dukket opp, var det vanskeligere å håndtere avhengigheter.

Alternativene til å starte et nytt prosjekt fra bunnen av inkluderer 'cloning' et eksisterende github prosjekt. Dette kan være mer effektivt hvis du vil bygge noe basert på eksisterende kodebibliotek eller rammer.

Når det gjelder implementeringsdetaljer, avhenger det av hva du vil gjøre. Du kan for eksempel velge å holde alle koden din i en enkelt fil, opprette separate filer for hver funksjon, eller bruke et modulsystem.

## Se Også
- [npm documentation](https://docs.npmjs.com/about-npm/)
- [MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide)
- [Github guides](https://guides.github.com/)
- [W3Schools Javascript Tutorial](https://www.w3schools.com/js/)