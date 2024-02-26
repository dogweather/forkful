---
date: 2024-01-20 17:41:48.721078-07:00
description: "\xC5 lage en midlertidig fil er en m\xE5te \xE5 lagre data midlertidig\
  \ under kj\xF8ring av et program. Programmerere gj\xF8r dette for \xE5 behandle\
  \ data uten \xE5 p\xE5virke\u2026"
lastmod: '2024-02-25T18:49:38.730788-07:00'
model: gpt-4-1106-preview
summary: "\xC5 lage en midlertidig fil er en m\xE5te \xE5 lagre data midlertidig under\
  \ kj\xF8ring av et program. Programmerere gj\xF8r dette for \xE5 behandle data uten\
  \ \xE5 p\xE5virke\u2026"
title: Opprette en midlertidig fil
---

{{< edit_this_page >}}

## What & Why?
Å lage en midlertidig fil er en måte å lagre data midlertidig under kjøring av et program. Programmerere gjør dette for å behandle data uten å påvirke permanente filer eller for å minimere bruk av minne.

## How to:
TypeScript har ikke innebygd støtte for å lage midlertidige filer, men vi kan bruke Node.js-biblioteker som `fs` og `tmp`.

```typescript
import * as fs from 'fs';
import * as tmp from 'tmp';

// Lager en midlertidig fil og skriver til den
tmp.file((error, path, fd, cleanupCallback) => {
  if (error) throw error;

  fs.writeSync(fd, 'Hei, dette er litt midlertidig data!');
  
  // Logg filstien
  console.log(`Midlertidig fil opprettet på: ${path}`);
  
  // Rydd opp
  cleanupCallback();
});

// Sample output:
// Midlertidig fil opprettet på: /tmp/tmp-1234abc
```
Merk at du må installere `tmp`-pakken fra npm (`npm install tmp`).

## Deep Dive
Før skytjenester ble vanlige, brukte programmerere ofte midlertidige filer på lokalt lagringsmedia. Med tiden har alternative løsninger som in-memory databaser og caches blitt mer populære.

Alternativer til midlertidige filer inkluderer:

1. In-memory lagring: Bruker RAM til midlertidig data, som er raskere å skrive til og lese fra.
2. Databaser: Bruk databaser til å lagre midlertidig data med mer avansert tilgangskontroll og spørringsmuligheter.

Når det gjelder implementasjon, bruker `tmp`-pakken et enkelt API for å håndtere midlertidige filer. Pakken tar seg av filoppretting og -sletting samt gir unike navn for å unngå kollisjoner.

## See Also
- Node.js `fs` dokumentasjon: https://nodejs.org/api/fs.html
- `tmp`-pakke på npm: https://www.npmjs.com/package/tmp
- Artikel om in-memory lagring alternativer: https://www.infoworld.com/article/3267744/when-to-use-in-memory-databases.html
