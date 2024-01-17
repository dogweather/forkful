---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Javascript: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Sjekk om en mappe eksisterer er en måte for programmere å sjekke om en spesifikk mappe finnes på en datamaskin eller server. Dette er nyttig når man ønsker å utføre forskjellige handlinger basert på om en mappe er tilgjengelig eller ikke.

# Hvordan:
```Javascript
if (fs.existsSync(directoryPath)) {
  console.log("Mappen eksisterer!");
} else {
  console.log("Mappen finnes ikke.");
}
```
I dette eksempelet bruker vi funksjonen `fs.existsSync()` for å sjekke om en mappe finnes på den angitte banen `directoryPath`. Hvis mappen eksisterer, blir det skrevet ut en melding som sier "Mappen eksisterer!" ellers vil det bli skrevet ut "Mappen finnes ikke." Ved hjelp av denne funksjonen kan man enkelt utføre forskjellige handlinger basert på om en mappe eksisterer eller ikke.

# Dykk dypere:
Historisk sett, før introduksjonen av Node.js i 2009, var det vanskelig å sjekke om en mappe eksisterte i Javascript. Dette skyldtes begrensninger med testing av filsystemet i nettlesere. Med Node.js, som har innebygde filsystemmoduler, ble det enklere å sjekke om en mappe eksisterer.

Alternativer til å bruke `fs.existsSync()` inkluderer bruk av `fs.accessSync()` eller `fs.statSync()`. Disse funksjonene kan gi mer detaljert informasjon om en fil eller mappe, men de kan også være mer komplekse å implementere.

Implementeringsdetaljer for å sjekke om en mappe eksisterer kan variere basert på operativsystemet. I Windows vil for eksempel backslash (`\`) bli brukt som skilletegn i en filbane, mens i Unix-systemer vil en forward slash (`/`) bli brukt. Det er derfor viktig å sørge for at filbanen blir angitt riktig for å kunne utføre en vellykket sjekk.

# Se også:
- Dokumentasjon for fs.existsSync(): https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Alternativer for å sjekke om en mappe eksisterer i Javascript: https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js