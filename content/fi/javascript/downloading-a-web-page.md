---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Artikkeli Aiheena: Kuinka Ladataan Verkkosivu HTML JavaScriptilla

## Mikä & Miksi?

Webbisivun lataaminen tarkoittaa sivun sisällön hakemista verkon yli ja sen näyttämistä selaimessa. Koodarit tekevät tämän esimerkiksi tietojen kaapimiseksi tai offline-sisällön luomiseksi.

## Näin Se Tehdään:

```Javascript
const https = require('https');
const fs = require('fs');

let url = "https://omanetajanosoite.com";

https.get(url, (res) => {
  res.pipe(fs.createWriteStream('sisalto.html'));
}).on('error', (e) => {
  console.error(`Virhe: ${e.message}`);
});
```

## Syvemmälle

**Historiallinen tausta:** 
Verkkosivujen lataaminen on ollut osa verkon perustoimintoa siitä saakka, kun ensimmäiset verkkosivut ja selaimet keksittiin 1990-luvun alussa.

**Vaihtoehdot:**
JavaScriptin lisäksi voit käyttää myös muita ohjelmointikieliä, kuten Python tai Ruby, verkkosivujen lataamiseen.

**Toteutus yksityiskohdat:**
`https.get()` -metodi hakee dataa antamastasi URL-osoitteesta. `res.pipe(fs.createWriteStream('sisalto.html'));` tallentaa tämän datan 'sisalto.html' tiedostoon.

## Katso Myös

- Node.js ja HTTP(S) moduulien dokumentaatio: https://nodejs.org/api/http.html
- fs-moduulin dokumentaatio: https://nodejs.org/api/fs.html
- Muita tapoja verkkosivujen lataamiseen: https://www.npmjs.com/package/html-downloader