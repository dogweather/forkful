---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error on virheiden ja lokiviestien streami. Koodaajat käyttävät sitä raportoimaan ohjelman suorituksen aikaisia ongelmia ilman, että se sekoittuu pääohjelman tulosteeseen.

## How to:
```
// Kirjoittaminen standard erroriin Javascriptillä
console.error('Tapahtui virhe!');

// Esimerkkituloste konsolissa:
// Tapahtui virhe!
```

## Deep Dive
Historiallisesti standard error (stderr) erotettiin standard outputista (stdout), jotta virheet saatiin käsiteltyä eri tavoin. Javascriptissä `console.error()` näyttää lähetetyn viestin stderr-virrassa. Tämä on lineäärinen tapa välittää virhesanomia, mutta näppärä väline kehitysvaiheessa. Vaihtoehtoisesti voit kirjoittaa lokitiedoston tai käyttää kolmannen osapuolen kirjastoja, kuten Winston tai Bunyan, jotka tarjoavat monipuolisempaa lokinhallintaa.

## See Also
- MDN Web Docs, Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Node.js, Console Class: https://nodejs.org/api/console.html#console_console_error_data_args

Useissa ympäristöissä `console.error` voi olla mukautettavissa ja kytkettävissä osaksi laajempaa lokinhallintaa, mutta standardikäyttö konsoliin kirjoittamiseen on edelleen yleinen ja käyttökelpoinen.
