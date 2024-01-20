---
title:                "Skrive en tekstfil"
html_title:           "Javascript: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å skrive til en tekstfil er en vanlig oppgave for programmerere. Dette refererer til å lagre data til en fil på en datamaskin eller server. Det kan være nyttig for å lagre og organisere informasjon eller for å samhandle med andre programmer ved å utveksle data. 

## Hvordan:
For å skrive til en tekstfil i Javascript, kan vi bruke FileSystem biblioteket ved å kalle den innebygde "fs" modulen. Deretter kan vi bruke metoden "writeFile" for å skrive til filen med følgende syntaks: 

```Javascript
fs.writeFile('nyFil.txt', 'Dette er innholdet i filen.', function(err) {
  if (err) throw err;
  console.log('Filen er opprettet!');
});
```

Dette eksempelet lager en ny fil med navnet "nyFil.txt" og skriver innholdet "Dette er innholdet i filen." til filen. Denne metoden aksepterer en rekke forskjellige filtyper som kan spesifiseres som en del av filnavnet, for eksempel ".txt" for tekstfiler eller ".json" for JSON-filer.

## Dypdykk:
Måten å skrive til en tekstfil på har endret seg gjennom årene, og det er flere måter å gjøre det på avhengig av språk og plattform. I tillegg til å bruke FileSystem biblioteket i Javascript, kan man også bruke andre moduler som "stream" eller "write". Noen utviklere foretrekker også å bruke databaser som MySQL eller MongoDB for å lagre og hente data i stedet for å skrive til filer direkte.

## Se også:
Her er noen nyttige ressurser for å lære mer om å skrive til en tekstfil i Javascript: 
- [Node.js FileSystem dokumentasjon](https://nodejs.org/api/fs.html)
- [Tutorialspoint sin artikkel om skriving til filer i Node.js](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)