---
date: 2024-01-20 18:03:52.139592-07:00
description: "How to: La oss starte et nytt JavaScript-prosjekt. F\xF8rst, installer\
  \ Node.js og npm (Node package manager). Deretter, opprett prosjektet."
lastmod: '2024-03-13T22:44:41.184142-06:00'
model: gpt-4-1106-preview
summary: La oss starte et nytt JavaScript-prosjekt.
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## How to:
La oss starte et nytt JavaScript-prosjekt. Først, installer Node.js og npm (Node package manager). Deretter, opprett prosjektet:

```Javascript
// Installer Express med npm (etter å ha kjørt 'npm init' i prosjekt mappen)
npm install express

// Server.js
const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.send('Hei, verden!');
});

const PORT = 3000;
app.listen(PORT, () => {
  console.log(`Serveren kjører på port ${PORT}`);
});
```

Kjør `node server.js`, og besøk `localhost:3000` i nettleseren. Du vil se 'Hei, verden!'.

## Deep Dive:
JavaScript har kommet langt siden 1995. Utviklingen av Node.js i 2009 lot JavaScript kjøre på serveren, ikke bare i nettleseren. Dette åpnet en verden av muligheter for fullstendige JavaScript-stakker, som MEAN og MERN.

Et alternativ til npm er Yarn, en annen pakkebehandler som noen foretrekker for dens hurtighet og pålitelighet.

Når du setter opp prosjektet, kan det være nyttig å forstå modulsystemet i Node.js, 'require' og 'exports', for å bedre organisere og vedlikeholde koden.

## See Also:
- [Node.js Offisiell Dokumentasjon](https://nodejs.org/en/docs/)
- [npm Offisiell Nettside](https://www.npmjs.com/)
- [Express.js Guide](https://expressjs.com/en/guide/routing.html)
- [Yarn Pakkebehandler](https://yarnpkg.com/)
