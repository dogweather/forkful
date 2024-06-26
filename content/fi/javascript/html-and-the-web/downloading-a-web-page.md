---
date: 2024-01-20 17:44:10.401386-07:00
description: "How to: (Kuinka tehd\xE4:) Aikojen alussa tavallisimmin k\xE4ytettiin\
  \ XMLHttpRequest-objekti verkkosivujen lataamiseen JavaScriptill\xE4. Fetch API\
  \ on nyky\xE4\xE4n\u2026"
lastmod: '2024-04-05T22:51:11.095768-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Aikojen alussa tavallisimmin k\xE4ytettiin XMLHttpRequest-objekti\
  \ verkkosivujen lataamiseen JavaScriptill\xE4."
title: Verkkosivun lataaminen
weight: 42
---

## How to: (Kuinka tehdä:)
```javascript
// Node.js -esimerkki käyttäen 'axios' -kirjastoa.
const axios = require('axios');

async function lataaSivu(url) {
  try {
    const response = await axios.get(url);
    console.log(response.data);
  } catch (error) {
    console.error('Virhe ladattaessa sivua:', error);
  }
}

// Käytä funktiota
lataaSivu('https://example.com');
```
Output:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (Syväsukellus):
Aikojen alussa tavallisimmin käytettiin XMLHttpRequest-objekti verkkosivujen lataamiseen JavaScriptillä. Fetch API on nykyään moderni valinta; se on luvun (Promise) perusteinen ja helpompi käyttää.

Esimerkiksi `axios` on suosittu kirjasto, joka toimii sekä selaimessa että Node.js:ssä. Vaihtoehtoja on monia: `request` (nyt vanhentunut), `superagent`, `node-fetch` jne.

Tarkkoja yksityiskohtia tungosta riippuen, saatat tarvita lisää kuten virheenkäsittelyä, uudelleenyrityksiä tai otsikoiden hallintaa.

## See Also (Katso myös):
- Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Axios GitHub page: https://github.com/axios/axios
- You Don't Know JS (asynchronous & performance): https://github.com/getify/You-Dont-Know-JS
