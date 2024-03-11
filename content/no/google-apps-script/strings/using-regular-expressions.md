---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:34.342973-07:00
description: "Regul\xE6re uttrykk (regex) er m\xF8nstre brukt til \xE5 matche kombinasjoner\
  \ av tegn i strenger. Programmerere bruker dem til s\xF8king, redigering eller\u2026"
lastmod: '2024-03-11T00:14:13.820309-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk (regex) er m\xF8nstre brukt til \xE5 matche kombinasjoner\
  \ av tegn i strenger. Programmerere bruker dem til s\xF8king, redigering eller\u2026"
title: "Bruke regul\xE6re uttrykk"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk (regex) er mønstre brukt til å matche kombinasjoner av tegn i strenger. Programmerere bruker dem til søking, redigering eller manipulering av tekst og data, noe som gjør dem uunnværlige for mønstersøking og databehandling.

## Hvordan:

Å bruke regulære uttrykk i Google Apps Script er enkelt takket være JavaScript-basert syntaks. Her er hvordan du kan inkorporere regex i skriptene dine for vanlige oppgaver som søking og datavalidering.

### Søke i strenger

Anta at du vil finne ut om en streng inneholder et spesifikt mønster, for eksempel en e-postadresse. Her er et enkelt eksempel:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Funnet: " + found[0]);
  } else {
    Logger.log("Ingen e-post funnet.");
  }
}

// Eksempel på bruk
findEmailInText("Kontakt oss på info@example.com.");
```

### Datavalidering

Regulære uttrykk skinner når det kommer til datavalidering. Nedenfor er en funksjon som validerer en inngangsstreng for å sjekke om den overholder en enkel passordpolicy (minst én stor bokstav, én liten bokstav og minst 8 tegn).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Eksempel på utdata
Logger.log(validatePassword("Str0ngPass")); // Utdata: true
Logger.log(validatePassword("weak"));       // Utdata: false
```

## Dypdykk

Regulære uttrykk i Google Apps Script er arvet fra JavaScript, først standardisert i ECMAScript språkspesifikasjon i juni 1997. Selv om de er kraftfulle, kan de noen ganger føre til forvirrende og vanskelig å vedlikeholde kode, spesielt når de brukes for mye eller for komplekse mønstersøkeoppgaver som kanskje kan løses mer effektivt gjennom andre analysemetoder.

For eksempel, selv om du kan bruke regex for HTML- eller XML-analyse i en klemme, er dette generelt frarådet på grunn av de nøstede og intrikate strukturene til disse dokumentene. I stedet er verktøy spesielt designet for å analysere slike strukturer, som DOM-parser for HTML, mer pålitelige og lesbare.

Videre bør Google Apps Script-utviklere være oppmerksomme på potensielle ytelsesproblemer når de bruker komplekse regex-mønstre i stor-skala tekstmanipuleringsoppgaver, da regex-behandling kan være CPU-intensiv. I slike tilfeller kan det å bryte oppgaven ned i enklere deloppgaver eller bruke innebygde strengmanipulasjonsfunksjoner tilby en bedre balanse mellom ytelse og vedlikeholdbarhet.
