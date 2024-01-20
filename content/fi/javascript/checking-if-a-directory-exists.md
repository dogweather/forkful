---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:56:57.622248-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Tarkistamme, olemassaoko kansio, estääksemme virheitä ja varmistaaksemme, että datamme menee oikeaan paikkaan. Se on tärkeää tiedostojen käsittelyssä ja kun luodaan tiedostoja dynaamisesti.

## How to: - Näin teet sen:
```Javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log(`Directory exists: ${directoryPath}`);
    return true;
  } catch (error) {
    console.log(`Directory does not exist: ${directoryPath}`);
    return false;
  }
}

// Käytä näin:
checkDirectory('./path/to/your/directory').then(exists => console.log(`Exists: ${exists}`));
```
Tuloste voisi olla:
```
Directory exists: ./path/to/your/directory
Exists: true
```
tai
```
Directory does not exist: ./path/to/your/directory
Exists: false
```

## Deep Dive - Syväsukellus:
Node.js:ssä 'fs' (FileSystem) moduuli on perinteisesti käytetty tiedostojärjestelmän kanssa toimimiseen. `fs.access()` on moderni tapa tarkistaa, onko kansio olemassa. Aiemmin käytettiin `fs.exists()`, mutta se on vanhentunut. `fs.access()` on suositeltava, koska se tarjoaa enemmän joustavuutta ja seuraa paremmin Node.js:n "error-first" callback -filosofiaa.

Vaihtoehtoisesti voisi käyttää synkronista `fs.existsSync()`, jos tehdään yksinkertaista skriptiä eikä tarvitse huolehtia sovelluksen suorituskyvystä.

## See Also - Katso myös:
- Node.js File System Docs: https://nodejs.org/api/fs.html
- `fs.access` MDN web docs: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/File/Access
- Error-first Callbacks: http://fredkschott.com/post/2014/03/understanding-error-first-callbacks-in-node-js/ 

Tässä artikkelissa tutkit miten kansioita tarkistetaan Javascriptilla käyttäen Node.js:ää. Perusasiat ensin — vältä sovelluksen kaatumisia ja tiedosta missä datasi liikkuu. Tiedon jakaminen voi olla sujuvaa ja yksinkertaista, pitäydymme siis vain tarpeellisessa ja teemme koodistamme tehokasta ja virheetöntä.