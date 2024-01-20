---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Javascript: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekking om en mappe eksisterer i JavaScript handler om å verifisere tilstedeværelsen av en mappe innenfor filsystemet. Programmerere gjør dette for å unngå feil som oppstår når man forsøker å manipulere en ikke-eksisterende mappe.

## Hvordan:
Vi kan bruke Node.js 'fs' modul til å sjekke dette. Her er det et eksempel.

```Javascript
const fs = require('fs');

fs.access('/sti/til/mappen', (error) => {
  if (error) {
    console.log('Mappen finnes ikke.');
  } else {
    console.log('Mappen finnes!');
  }
});
```
Når koden er kjørt, vil du får enten "Mappen finnes!" eller "Mappen finnes ikke." basert på mappens eksistens i oppgitt sti.

## Dypdykk:
Historisk sett har JavaScript ikke hatt direkte tilgang til filsystemet, men med innføringen av Node.js ble dette mulig. Alternativer til 'fs' -modulen inkluderer 'graceful-fs', som legger til noen robuste funksjoner for å forhindre crash under lows av fil I/O operasjoner.

Metoden 'fs.access()' brukes her fordi det er asynkront, dvs. det blokkerer ikke andre operasjoner mens den sjekker. Denne metoden kaster bare en feil når mappen ikke finnes, noe som gjør den perfekt til vårt formål.

## Se Også:
1. Node.js fs Docs: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
2. Alternativer til Node.js fs: [https://www.npmjs.com/package/graceful-fs](https://www.npmjs.com/package/graceful-fs)
4. Generelle feilhåndtering i JavaScript: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Control_flow_and_error_handling](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Control_flow_and_error_handling)