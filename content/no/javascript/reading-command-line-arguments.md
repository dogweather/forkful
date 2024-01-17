---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Javascript: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Lesing av kommandolinjeargumenter er en vanlig oppgave for programmere. Det innebærer å lese og behandle verdier som er angitt som parametere når et program blir kjørt fra en terminal eller kommandolinjegrensesnitt. Dette er nyttig for å gi ekstra fleksibilitet til et program, for eksempel ved å tillate brukere å angi ulike verdier for å tilpasse programmet til deres behov.

## Hvordan:
For å lese kommandolinjeargumenter i Javascript, kan du bruke ```process.argv```-objektet. Dette objektet inneholder en liste over argumenter gitt til programmet, der det første argumentet er banen til node-executable og det andre argumentet er banen til Javascript-filen som blir kjørt. Deretter følger eventuelle andre argumenter som blir angitt. For å få tilgang til disse argumentene, kan du bruke indeksering på samme måte som med et vanlig array. 

Se et eksempel under for å lese og skrive ut kommandolinjeargumenter:

```Javascript
// Les og skriv ut første argument
console.log("Første argument: " + process.argv[2]);

// Les og skriv ut andre argument
console.log("Andre argument: " + process.argv[3]);
```

Kjøre programmet med følgende kommando vil gi følgende output:

```Javascript
$ node arguments.js argument1 argument2
Første argument: argument1
Andre argument: argument2
```

## Dypdykk:
Lesing av kommandolinjeargumenter er en gammel og velkjent oppgave i programmering. I tidligere dager var det vanlig å måtte skrive mye boilerplate-kode for å lese argumenter og gjøre noe nyttig med dem. I nyere versjoner av Javascript og andre programmeringsspråk, har dette blitt forenklet ved hjelp av dedikerte funksjoner og objekter som ```process.argv```.

Det finnes ulike alternativer for å lese og behandle kommandolinjeargumenter, som for eksempel pakken yargs. Dette kan være nyttig for mer komplekse applikasjoner som krever mer avansert håndtering av argumenter.

Det kan også være verdt å merke seg at kommandolinjeargumenter kan også leses og prosesseres ved hjelp av et brukergrensesnitt, som for eksempel en grafisk brukergrensesnitt eller webgrensesnitt. Dette kan være mer tilgjengelig for mindre erfarne brukere som ikke er vant til å bruke kommandolinjen.

## Se også:
- [Node.js dokumentasjon om process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [yargs pakken](https://www.npmjs.com/package/yargs)
- [Alternativer for brukergrensesnitt for å lese og behandle kommandolinjeargumenter](https://www.npmjs.com/search?q=command+line+interface)