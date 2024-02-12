---
title:                "Å bruke et interaktivt skall (REPL)"
aliases:
- /no/google-apps-script/using-an-interactive-shell-repl.md
date:                  2024-02-01T22:04:02.293163-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke et interaktivt skall (REPL)"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/google-apps-script/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Et interaktivt skall, eller Lese-Evaluere-Skrive Løkke (REPL), er et enkelt, interaktivt programmeringsmiljø som tar enkelte brukerinndata (uttrykk), evaluerer dem, og returnerer resultatet til brukeren. Programmerere bruker REPL-er for rask prototyping, feilsøking, og for å lære syntaksen og oppførselen til et programmeringsspråk på en interaktiv måte.

## Hvordan:

Google Apps Script, et skybasert skriptspråk for automatisering av oppgaver på tvers av Google-produkter, har ikke et innebygd REPL-verktøy som ligner på de i språk som Python eller JavaScripts Node.js. Imidlertid kan du simulere en lignende opplevelse ved å bruke logg- og feilsøkingsfunksjonene i Apps Script-redigereren eller ved å sette opp et eksternt miljø. Her fokuserer vi på å lage en improvisert REPL inne i Apps Script-redigereren.

1. **Lage en improvisert REPL-funksjon**:

```javascript
function myREPL() {
  var input = Logger.log('Skriv inn uttrykket ditt: ');
  try {
    var result = eval(input);
    Logger.log('Resultat: ' + result);
  } catch(e) {
    Logger.log('Feil: ' + e.message);
  }
}
```

Siden direkte brukerinndata ikke er gjennomførbart på samme måte som en tradisjonell REPL i Apps Script-miljøet, kan du manuelt endre `input`-variabelen og kjøre `myREPL()` for å teste uttrykk.

2. **Eksempel på kodeutførelse**:

La oss si at du ønsker å evaluere `2+2`. Da ville du endre `myREPL`-funksjonen slik:

```javascript
function myREPL() {
  var input = '2+2'; // Skriv inn uttrykket ditt manuelt her
  // Resten forblir det samme...
}
```

Etter å ha kjørt `myREPL()`, sjekk loggene (Vis > Logger) for utdata, som bør lese noe i retning av:

```
[20-xx-xxxx xx:xx:xx:xxx] Skriv inn uttrykket ditt:
[20-xx-xxxx xx:xx:xx:xxx] Resultat: 4
```

3. **Feilsøking med Logger**:

For mer kompleks feilsøking, interspers `Logger.log(variabel);` innenfor koden din for å skrive ut variabeltilstander, noe som hjelper deg å forstå flyten og mellomliggende tilstander i skriptene dine.

## Dypdykk

Konseptet med en REPL er dypt forankret i databehandlingens historie, som stammer fra tidsdelingssystemene på 1960-tallet som tillot interaktive økter. Språk som Lisp trivdes i dette miljøet, ettersom REPL var kritisk for deres iterative utviklingsprosess. I kontrast er Google Apps Script, som dukket opp mye senere, designet primært for nettet, med fokus på automatisering av oppgaver i Googles suite fremfor iterativ, konsollbasert programmering.

Google Apps Script støtter tradisjonelt ikke sanntids, interaktive kodingssesjoner ut av boksen på grunn av sin skybaserte natur og webapplikasjonsfokus. Dets utførelsesmodell kretser rundt funksjoner utløst av webhendelser, tidsdrevne utløsere, eller manuell påkalling innen miljøet, snarere enn øyeblikkelige tilbakemeldingssløyfer levert av en REPL.

Selv om den improviserte REPLen og feilsøkeren inne i Apps Script-redigereren tilbyr et visst nivå av interaktivitet, replikerer de ikke umiddelbarheten og effektiviteten til tradisjonelle REPLer funnet i mange programmeringsspråk. Utviklere som søker en mer autentisk REPL-opplevelse med Googles teknologier, kan utforske eksterne JavaScript-miljøer eller Node.js med Googles APIer. Disse kan tilby en mer responsiv og interaktiv kodingsøkt, selv om det krever mer oppsett og potensielt et steg bort fra det direkte Apps Script-miljøet.
