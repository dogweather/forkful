---
title:                "Verkkosivun lataaminen"
date:                  2024-01-20T17:44:10.401386-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/downloading-a-web-page.md"
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
