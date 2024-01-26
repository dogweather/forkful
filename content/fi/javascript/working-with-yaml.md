---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä & Miksi?"
YAML on ihmisen luettava datan serialisointikieli, jota käytetään konfiguraatiotiedostoihin ja datan tallentamiseen. Ohjelmoijat käyttävät YAMLia sen selkeyden ja helpon luettavuuden vuoksi, mikä nopeuttaa ja yksinkertaistaa konfiguraatioiden hallintaa.

## How to:
"Kuinka:"
Javascript-kirjasto `js-yaml` on helppo tapa käsitellä YAML-tiedostoja. Tässä esimerkki kuinka luetaan ja muunnetaan YAML JavaScript-objektiksi.

```javascript
const yaml = require('js-yaml');
const fs   = require('fs');

try {
  // Lue YAML-tiedosto
  const data = fs.readFileSync('config.yaml', 'utf8');
  // Muunna YAML JavaScript-objektiksi
  const config = yaml.load(data);
  console.log(config);
} catch (e) {
  console.error(e);
}
```
Jos `config.yaml` sisältää:
```yaml
version: 1
services:
  webapp:
    image: "my-webapp:latest"
    ports:
      - "8080:80"
```
Tulostus olisi:
```json
{ version: 1, services: { webapp: { image: 'my-webapp:latest', ports: [ '8080:80' ] } } }
```

## Deep Dive
"Syväsukellus":
YAML (YAML Ain't Markup Language) lanseerattiin 2001, ja se on kehittynyt yhdeksi suosituimmista konfiguraatiokieliä erityisesti DevOps-kulttuurissa. YAMLin vaihtoehtoja ovat JSON ja XML, jotka ovat myös laajasti käytettyjä datan serialisointikieliä. Yksi YAMLin implementaation erikoisuuksia on sen kyky käsitellä monimutkaisia datarakenteita, kuten listoja ja hajautustauluja, luonnollisella ja loogisella tavalla.

## See Also
"Näihin kannattaa tutustua myös":
- YAML virallinen sivusto: https://yaml.org/
- `js-yaml` GitHub-sivu: https://github.com/nodeca/js-yaml
- YAML vs. JSON vertailu: https://phoenixnap.com/kb/yaml-vs-json-vs-xml
