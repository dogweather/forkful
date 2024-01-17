---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Javascript: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Mitä ja Miksi?

Kirjoittaminen standardivirheen ulostuloon on tapa koodata, jossa käyttäjä voi tulostaa virheviestejä ohjelman suorituksen aikana. Tämä on hyödyllinen tapa selventää ohjelman toimintaa ja auttaa kehittäjiä tunnistamaan ja korjaamaan mahdollisia virheitä.

# Miten?

```Javascript
console.error("Tämä on virheviesti");
```

**Tulostus:** Tämä on virheviesti

Voit myös lisätä muita tietoja viestiin, kuten:

```Javascript
console.error("Virhe sivulla " + sivunNimi + ": " + virheKoodi);
```

**Tulostus:** Virhe sivulla Etusivu: 404

# Syväsukellus

Kirjoittaminen standardivirheen ulostuloon on osa virheenkäsittelyä ohjelmoinnissa. Ennen standardivirheen käyttöönottoa, kehittäjät joutuivat käsittelemään virheitä vaihtoehtoisilla tavoilla, kuten kirjoittamalla ne konsoliin tai tallentamalla ne lokitiedostoon.

On myös olemassa muita tapoja käyttää standardivirhettä, kuten käytettäessä ns. error event listeneria. Tämä mahdollistaa muiden virheiden kuuntelemisen, kuten verkkovirheet, ja virheiden hallitsemisen eri tavalla.

# Tutustu myös

Voit lukea lisää standardivirheestä ja sen käytöstä täältä: [MDN web docs - console.log()](https://developer.mozilla.org/en-US/docs/Web/API/console/error)