---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lesing av kommandolinjeargumenter i TypeScript

## Hva & Hvorfor?

Kommandolinjeargumenter er de dataene du gir til et program når du starter det i et kommandolinjemiljø. Vi bruker det for å kontrollere run-time oppførsel av programmet.



## Hvordan gjøre:

Husk at den første argumentet alltid er stien til din Node.js kjerne, og det andre argumentet er stien til utførelsesfilen. Så de reelle argumentene starter fra indeks 2.

```TypeScript
let myArgs = process.argv.slice(2);
console.log(myArgs);
```
Du kan kjøre programmet ditt med argumenter som dette: 
```
$ node myfile.js one two=three four
```
Output vil være:
```
[ 'one', 'two=three', 'four' ]
```

Du kan også parse argumentene etter eget behov. For eksempel:

```TypeScript
let myArgs = process.argv.slice(2);
let argsParsed = myArgs.map(arg => arg.split('='));
console.log(argsParsed);
```
Kjør programmet ditt med argumentene som ovenfor, vil output nå være:
```
[ [ 'one' ], [ 'two', 'three' ], [ 'four' ] ]
```


## Dyp Dykk

Historisk sett, lesing av kommandolinjeargumenter er stammer fra Unix og Linux systemer, før GUI-er ble utbredt. Til tross for fremveksten av mer brukervennlige grensesnitt, forblir kommandolinjeargumentlesing en viktig teknikk, særlig for utvikling av skript og automatiserte oppgaver.

I Node.js, er det alternative måter å håndtere kommandolinjeargumenter på. Noen populære biblioteker som minimist, yargs, og commander.js kan hjelpe med parsing og validering.

Ved implementering, vær oppmerksom på at `process.argv` er en globalt tilgjengelig array innenfor Node.js miljøet som alltid er tilgjengelig for deg. Men det er viktig å vite at argumentene er alltid strenger. Selv tall må konverteres før du behandler dem som numeriske verdier.


## Se også:

For mer om dette emnet, se følgende linker:

[Node.js dokumentasjon, process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)

[yargs, et kraftig argument parsing biblioteket for Node.js](https://www.npmjs.com/package/yargs)

[minimist, et minimalistisk Node.js argument parsing biblioteket](https://www.npmjs.com/package/minimist)

[commander.js, en fullfunksjonert Node.js kommandolinje grensesnitt](https://www.npmjs.com/package/commander)