---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:24.474645-07:00
description: "Hvordan: JavaScript tilbyr `Date.parse()`-metoden og `Date`-konstrukt\xF8\
  ren for \xE5 parse datostrenger. Men, disse tiln\xE6rmingene har begrensninger og\u2026"
lastmod: '2024-03-13T22:44:41.192751-06:00'
model: gpt-4-0125-preview
summary: "JavaScript tilbyr `Date.parse()`-metoden og `Date`-konstrukt\xF8ren for\
  \ \xE5 parse datostrenger."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
JavaScript tilbyr `Date.parse()`-metoden og `Date`-konstruktøren for å parse datostrenger. Men, disse tilnærmingene har begrensninger og inkonsistenser på tvers av ulike nettlesere, spesielt med ikke-standard datofomater. For å løse disse problemene er tredjepartsbiblioteker som `Moment.js` og `date-fns` populære på grunn av deres robusthet og enkel bruk.

### Bruke innfødt JavaScript:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // Utdata: Sun Apr 30 2023 14:55:00 GMT+0000 (Koordinert universaltid)
```

### Bruke Moment.js:
Først, installer Moment.js via npm eller inkluder det i prosjektet ditt. Deretter:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // Utdata: Sun Apr 30 2023 14:55:00 GMT+0000
```

### Bruke date-fns:
Etter å ha lagt til `date-fns` i prosjektet ditt, parse en datostreng slik:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // Utdata: 2023-04-30T14:55:00.000Z
```

Både `Moment.js` og `date-fns` tilbyr mer omfattende parseringsmuligheter, inkludert håndtering av en rekke formater og lokaliteter, noe som gjør dem foretrukket for komplekse applikasjoner.
