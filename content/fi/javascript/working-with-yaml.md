---
title:                "Javascript: Työskentely yaml:n kanssa."
simple_title:         "Työskentely yaml:n kanssa."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi Käyttää YAMLia JavaScript-Ohjelmoinnissa?

YAML (YAML Ain't Markup Language) on formaatti, jota käytetään tallentamaan tietoa tekstimuodossa. Se on suosittu monilla eri kielillä, mukaan lukien JavaScript, ja sillä on useita käyttötarkoituksia, kuten tietokantojen rakentaminen, konfiguraatiotiedostojen tallentaminen ja tietojen vaihtaminen eri sovellusten välillä. Käyttämällä YAMLia, voit helposti tallentaa ja lukea tietoja JavaScript-koodissasi.

## Miten Käyttää YAMLia JavaScriptin Kanssa?

YAMLin käyttäminen JavaScriptissä on helppoa, sillä siihen löytyy useita kirjastoja, kuten js-yaml ja yaml-js. Näiden avulla voit helposti lukea YAML-muotoisia tiedostoja ja muuttaa ne JavaScript-objekteiksi. Seuraavassa on esimerkki siitä, miten voit lukea YAML-tiedoston ja tulostaa sen sisällön konsoliin käyttäen js-yaml-kirjastoa:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

// Lukee YAML-tiedoston ja tallentaa sen JavaScript-objektina
const data = yaml.safeLoad(fs.readFileSync('tiedosto.yaml', 'utf8'));

// Tulostaa objektin konsoliin
console.log(data);
```

Esimerkiksi, jos tiedostossasi on seuraava YAML-data:

```yaml
nimi: Johanna
ika: 25
kieli: suomi
```

Koodi tulostaa seuraavan objektin konsoliin:

```javascript
{ nimi: 'Johanna', ika: 25, kieli: 'suomi' }
```

Voit myös luoda uuden YAML-tiedoston tai kirjoittaa tietoja olemassa olevaan tiedostoon käyttämällä yaml-js-kirjastoa. Seuraavassa on esimerkki siitä, miten voit luoda uuden YAML-tiedoston ja tallentaa siihen tietoja:

```javascript
const YAML = require('yaml-js');
const fs = require('fs');

// Luo uuden YAML-tiedoston ja tallentaa siihen tietoja
const data = { nimi: 'Matti', ika: 32, kieli: 'suomi' };
const yamlData = YAML.dump(data);

fs.writeFileSync('uusi_tiedosto.yaml', yamlData);
```

## Syvempi Sukellus YAMLin Käyttöön

Jos haluat oppia enemmän YAMLin käytöstä JavaScriptissä, voit tutustua js-yamlin ja yaml-js:n dokumentaatioon, jotka antavat sinulle tarkemman kuvauksen eri toiminnoista ja vaihtoehdoista. Voit myös käyttää YAMLin kanssa muita kirjastoja ja työkaluja, kuten YAML-validaattoria, joka auttaa varmistamaan, että YAML-tiedostosi ovat oikeassa muodossa ja eivät aiheuta ongelmia koodissasi.

## Katso Myös

- [js-yaml dokumentaatio](https://github.com/nodeca/js-yaml)
- [yaml-js dokumentaatio](https://github.com/yaml-js/yaml-js)
- [YAML-validaattori](https://jsonformatter.org/yaml-validator)