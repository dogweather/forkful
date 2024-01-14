---
title:    "Javascript: Lesing av kommandolinje-argumenter"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

I denne blogginnlegget skal vi ta en nærmere titt på hvordan man kan lese inn kommandolinjeargumenter i Javascript. Dette kan være nyttig for programmerere som ønsker å gjøre interaksjon med brukeren mer fleksibel, eller for å kunne automatisk kjøre et program med ulike inputverdier.

## Hvordan

Vi kan lese inn kommandolinjeargumenter i Javascript ved å bruke `process.argv` objektet. Dette objektet inneholder en array med alle de argumentene som er sendt inn ved å kjøre programmet. La oss se på et eksempel:

```Javascript
let args = process.argv;
console.log(args);
```
Eksempel input i terminalen:
```bash
node program.js arg1 arg2 arg3
```
Eksempel output:
```
[ 'node', '/Users/bruker/Desktop/program.js', 'arg1', 'arg2', 'arg3' ]
```
Som vi kan se, inneholder `process.argv` en liste over argumentene, hvor det første argumentet er selve Node-kommandoen, fulgt av stien til programmet og deretter de angitte argumentene. Vi kan også spesifisere hvilken indeks i arrayet vi ønsker å hente ut ved å bruke `process.argv[index]`.

Vi kan også bruke et tredjeparts bibliotek som `yargs` for å gjøre håndteringen av kommandolinjeargumenter enklere og mer strukturert. Dette biblioteket lar oss spesifisere hvilke argumenter som forventes, og gir mulighet for å legge til flagg og alternativer.

## Dypdykk

Når vi leser inn kommandolinjeargumenter, er det viktig å være bevisst på at disse blir levert som strenger. Dette betyr at dersom vi ønsker å bruke dem som tall i vår kode, må vi konvertere dem fra strenger til tall ved hjelp av funksjoner som `parseInt()` eller `parseFloat()`.

Det kan også være lurt å håndtere eventuelle feil i inputen ved å validere argumentene før de brukes i koden. Dette kan gjøres ved å bruke conditional statements eller ved å implementere en egen funksjon for validasjon.

## Se Også

- [Node.js process.argv documentation](https://nodejs.org/api/process.html#process_process_argv)
- [yargs documentation](https://www.npmjs.com/package/yargs)
- [parseInt() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt)
- [parseFloat() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseFloat)