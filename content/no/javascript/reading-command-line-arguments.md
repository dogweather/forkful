---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å lese kommandolinjeargumenter er prosessen med å ta inn ytterligere inndata fra brukeren når et program blir utført. Vi programmerere gjør dette for å gjøre programmene våre mer fleksible og tilpasningsdyktige til brukerens behov.

##Slik gjør du det:
Å bruke Node.js sin global `process.argv` er en vanlig metode til å lese kommandolinjeargumenter. Eksempel på kode og utskrift er vist under:

```Javascript
// myScript.js
console.log(process.argv)

```

For å kjøre dette, skriv inn `node myScript.js arg1 arg2` i kommandolinjen. 

Utskriften vil bli:

```Javascript
[
  '/path/to/node',
  '/path/to/myScript.js',
  'arg1',
  'arg2'
]
```
## Dyp Dykk
For det første, det å lese kommandolinjeargumenter har en lang historie som strekker seg tilbake til de tidligste dagene av programmering. Før grafiske brukergrensesnitt, var kommandolinjeargumenter hovedmåten programmer mottok inndata på.

Alternativt kan man også bruke tredjeparts integrerte biblioteker som 'yargs' eller 'commander' for å forenkle og strukturere kommandolinje parsing.

Når det kommer til implementeringsdetaljer, er `process.argv` en array i globalt omfang. Den inneholder kommandolinje argumenter som ble gitt til Node.js prosessen. `process.argv[0]` er alltid plasseringen til Node.js, og `process.argv[1]` er alltid plasseringen til den kjørte filen. Derfor starter våre egne argumenter på `process.argv[2]`.

## Se også
For mer informasjon, se følgende ressurser:

- [Node.js dokumentasjon](https://nodejs.org/docs/latest/api/process.html#process_process_argv)

- [yargs - Node.js argument parsert](https://github.com/yargs/yargs)

- [commander - Node.js kommandolinje grensesnitt](https://github.com/tj/commander.js)