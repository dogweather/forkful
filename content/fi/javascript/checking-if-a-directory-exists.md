---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Javascript: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Tarkista, Onko Hakemisto Olemassa JavaScriptilla?

## Mikä & Miksi?
Tarkistamme, onko hakemisto olemassa estääksemme virheiden tapahtumisen, kuten tiedoston kirjoittamisen olemattomiin hakemistoihin. Tämä parantaa sovelluksen vakautta ja käyttäjän kokemusta.

## Miten tehdään:
Käytämme Node.js: n File System -moduulia (fs). Siinä on `existsSync()`-funktio jolla voimme tarkistaa, onko hakemisto olemassa.

```Javascript
const fs = require('fs');

if(fs.existsSync('example_directory')) {
    console.log('Hakemisto on olemassa.');
} else {
    console.log('Hakemisto ei ole olemassa.');
}
```
Toiminnon suorittamisen jälkeen tulostuu joko 'Hakemisto on olemassa.' tai 'Hakemisto ei ole olemassa.' riippuen siitä, onko määritetty hakemisto olemassa.

## Syvällinen sukellus
Historiallisessa yhteydessä `exists()`-funktiota on käytetty hakemistojen olemassaolon tarkistamiseen, mutta se on vanhentunut. Se korvattiin `existsSync()` toiminnolla, joka on synkroninen versio.

Vaihtoehtona on käyttää `statSync()` tai `accessSync()`, mutta nämä funktiot heittävät virheitä, jos hakemistoa ei ole, mikä tekee koodista monimutkaisemman. 

`existsSync()`:n toteutus yksinkertaisesti tarkistaa, onko tietty polku tiedostojärjestelmässä olemassa ilman virheitä.

## Katso myös
Vieraile [Node.js-dokumentaatiosta](https://nodejs.org/api/fs.html) saadaksesi lisätietoja File System -moduulista.

Tarkastele [Stack Overflow](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)-keskustelua, joka selittää miksi `exists()` on vanhentunut ja keskustelee siitä, kuinka tarkistaa olemassaolo synkronisesti.