---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "Javascript: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinje-argumenter er en essensiell del av JavaScript-programmering. Dette gir utviklere muligheten til å interagere med brukeren og skrive dynamiske skript som kan kjøres i nettleseren eller på serveren.

## Hvordan

For å lese kommandolinje-argumenter må du bruke et innebygd objekt i JavaScript kalt "process". Dette objektet gir tilgang til informasjon om den nåværende prosessen, inkludert kommandolinje-argumenter. For å lese argumentene, kan du bruke "process.argv" -metoden som returnerer en matrise med alle argumentene gitt ved kjøring av skriptet.

``` Javascript
// Eksempel på å lese kommandolinje-argumenter

// Skriv ut alle argumentene i terminalen
for (let i = 0; i < process.argv.length; i++) {
  console.log(process.argv[i]);
}

/* Output:
path/to/node
path/to/myScript.js
Hello
World 
*/
```

Hvis du vil ha et spesifikt argument, kan du bruke indeksering på matrisen. Det første argumentet (indeks 0) vil alltid være banen til Node.js-programmet som kjører skriptet, mens det andre argumentet (indeks 1) vil være banen til skriptet selv.

## Dypdykk

Det er viktig å merke seg at kommandolinje-argumenter alltid returneres som strenger, uavhengig av om du skriver et tall eller en boolean-verdi som argument. Dette betyr at du må bruke konverteringsmetoder som "parseInt" eller "parseFloat" hvis du vil bruke argumentet som et tall i koden din. Det er også mulig å sende inn egendefinerte argumenter ved å bruke flagg i kommandolinjen, for eksempel "-f" for å angi en fil som skal åpnes.

## Se også

- [Node.js process dokumentasjon] (https://nodejs.org/dist/latest-v14.x/docs/api/process.html)
- [Mozilla Developer Network - Command line arguments] (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/Command_line_arguments)