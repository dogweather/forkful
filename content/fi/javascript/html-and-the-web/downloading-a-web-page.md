---
date: 2024-01-20 17:44:10.401386-07:00
description: "Lataamme verkkosivun, kun haluamme hakea sen sis\xE4lt\xF6\xE4 koodillamme.\
  \ T\xE4m\xE4 mahdollistaa tiedon k\xE4sittelyn, tallentamisen tai hy\xF6dynt\xE4\
  misen automaattisissa\u2026"
lastmod: 2024-02-19 22:05:15.843636
model: gpt-4-1106-preview
summary: "Lataamme verkkosivun, kun haluamme hakea sen sis\xE4lt\xF6\xE4 koodillamme.\
  \ T\xE4m\xE4 mahdollistaa tiedon k\xE4sittelyn, tallentamisen tai hy\xF6dynt\xE4\
  misen automaattisissa\u2026"
title: Verkkosivun lataaminen
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lataamme verkkosivun, kun haluamme hakea sen sisältöä koodillamme. Tämä mahdollistaa tiedon käsittelyn, tallentamisen tai hyödyntämisen automaattisissa prosesseissa.

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
