---
title:                "Sjekke om en mappe eksisterer"
html_title:           "TypeScript: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Sjekking av om en mappe eksisterer er en vanlig oppgave for programmører, spesielt når de jobber med filbehandling. Dette er en viktig sjekk for å sikre at programmene våre fungerer som forventet og at vi unngår feilkoder når vi forsøker å få tilgang til en mappe som ikke eksisterer.

## Hvordan:
```TypeScript
if(fileSystem.existsSync(path.dirname)){
  console.log("Mapper eksisterer!");
} else {
  console.log("Mappe finnes ikke!");
}
```
Eksempel på utdata:
```
Mapper eksisterer!
```

## Dypdykk:
Sjekking av om en mappe eksisterer kan gjøres på ulike måter, avhengig av programmeringsspråk og operativsystem. I eldre versjoner av JavaScript kunne man bruke `fs.existsSync()` metoden, men denne er nå markert som utdatert og bør unngås. I TypeScript bruker vi heller `fileSystem.existsSync()` som en del av Node.js' File System-modul.

Det finnes også alternative måter å sjekke om en mappe eksisterer på, som for eksempel `fs.stat()` eller `fs.access()`. Disse metodene kan også brukes til å få mer informasjon om mappen, som for eksempel når den ble opprettet eller siste gang den ble endret.

Når vi sjekker om en mappe eksisterer, bør vi også ta hensyn til eventuelle sikkerhetsbegrensninger eller tillatelser som kan hindre tilgang til mappen. Dette gjelder spesielt når vi jobber med sensitive filer eller nettverksmapper.

## Se også:
- Node.js File System-modul: https://nodejs.org/api/fs.html
- Sammenligning av ulike metoder for å sjekke om en mappe eksisterer: https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js