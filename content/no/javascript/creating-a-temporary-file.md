---
title:    "Javascript: Opprettelse av en midlertidig fil"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer er en viktig del av Javascript-programmering, spesielt når man jobber med større og mer komplekse prosjekter. Disse filene er nyttige for å lagre midlertidig data eller informasjon for å utføre forskjellige oppgaver, og kan raskt opprettes og slettes når de ikke lenger er nødvendige.

## Hvordan

Det finnes flere måter å lage midlertidige filer i Javascript, men den enkleste og mest effektive metoden er å bruke "fs" (file system) modulen i Node.js. Her er et eksempel på hvordan du kan opprette en midlertidig fil ved hjelp av denne modulen:

```Javascript
var fs = require('fs');
// Opprett en midlertidig fil med navnet "temp.txt"
fs.open('temp.txt', 'w', function(err, file) {
    if (err) throw err;
    console.log('Midlertidig fil opprettet');
});
```

I dette eksempelet bruker vi "fs.open" funksjonen til å opprette en fil med navnet "temp.txt". Vi spesifiserer også at filen skal være åpen for skriving ("w"), og dersom det oppstår en feil, kastes denne med "throw err". Hvis alt går som det skal, vil du se meldingen "Midlertidig fil opprettet" i konsollen.

Når filen er opprettet, kan du skrive data til den og lese data fra den på samme måte som du ville gjort med en vanlig fil. Husk bare å slette den når den ikke lenger er nødvendig ved å bruke "fs.unlink" funksjonen.

```Javascript
// Skriv data til den midlertidige filen
var data = "Dette er data som skal skrives til den midlertidige filen";
fs.writeFile('temp.txt', data, function(err) {
    if (err) throw err;
    console.log('Data skrevet til filen');
});

// Les data fra den midlertidige filen
fs.readFile('temp.txt', 'utf8', function(err, data) {
    if (err) throw err;
    console.log('Data lese fra filen: ' + data);
});

// Slett den midlertidige filen
fs.unlink('temp.txt', function(err) {
    if (err) throw err;
    console.log('Midlertidig fil slettet');
});
```

## Dypdykk

Når du lager midlertidige filer i Javascript, er det viktig å være oppmerksom på sikkerhet og ytelse. Det er alltid best å begrense bruken av midlertidige filer, spesielt hvis du jobber med store datamengder, for å unngå eventuelle ytelsesproblemer.

I tillegg er det viktig å sørge for at midlertidige filer slettes når de ikke lenger er nødvendige. Dette kan gjøres ved hjelp av både "fs.unlink" funksjonen som vist i eksempelet over, og også ved å bruke "temp" funksjonen i Node.js for å opprette midlertidige filer i en midlertidig mappe.

## Se også

- [Node.js Dokumentasjon - File System](https://nodejs.org/dist/latest-v10.x/docs/api/fs.html)
- [MDN Web Docs - File System API](https://developer.mozilla.org/en-US/docs/Web/API/File_and_Directory_APIs)