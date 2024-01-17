---
title:                "Konvertering av dato til tekst"
html_title:           "TypeScript: Konvertering av dato til tekst"
simple_title:         "Konvertering av dato til tekst"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av en dato til en streng er en vanlig praksis blant programmerere for å endre formatet på en dato som er lagret i systemet. Dette gjøres vanligvis for å gjøre det lettere å lese og presentere datoer på en mer visuelt tiltalende måte for brukerne.

## Hvordan:

```TypeScript
const dato = new Date();
const strengDato = dato.toLocaleString();
console.log(strengDato); // Output: 12. desember 2020 kl. 18:53:00
```

I dette eksempelet bruker vi `new Date()` for å opprette en ny dato-objekt og `toLocaleString()` for å konvertere den til en lokaliseringsavhengig presentasjon av dato og klokkeslett. Deretter bruker vi `console.log()` for å skrive ut konvertert dato til konsollen.

## Dypdykk:

Å konvertere en dato til en streng har vært en del av programmering i lang tid. Tidligere ble det gjort manuelt ved å separere dato og klokkeslett og formatering av hver del separat. Med utviklingen av programmeringsspråk som TypeScript, har det blitt mye enklere å konvertere en dato til en streng ved hjelp av innebygde funksjoner som `toLocaleString()`.

En alternativ måte å konvertere en dato til en streng på er å bruke en tredjeparts bibliotek som moment.js eller date-fns. Disse bibliotekene tilbyr flere avanserte funksjoner for formatering og manipulering av datoer.

Når man konverterer en dato til en streng, er det viktig å være oppmerksom på den lokale kulturen og konvensjoner for datoformatering. Dette kan variere mellom ulike land og språk, og derfor bør man alltid bruke funksjoner som `toLocaleString()` for å sikre en korrekt presentasjon av datoene.

## Se også:

- [MDN web docs - Date.prototype.toLocaleString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)