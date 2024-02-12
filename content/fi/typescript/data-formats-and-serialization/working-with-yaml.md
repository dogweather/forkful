---
title:                "Työskentely YAML:n kanssa"
aliases:
- /fi/typescript/working-with-yaml.md
date:                  2024-02-03T19:27:18.898952-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
YAML, ihmisläheisesti suunniteltu tietojen serialisointikieli, on usein käytössä konfiguraatiotiedostoissa, prosessien välisessä viestinnässä ja tietojen tallennuksessa. Ohjelmoijat suosivat YAMLia sen luettavuuden ja helppokäyttöisyyden vuoksi, erityisesti käsiteltäessä monimutkaista rakenteistettua tietoa, mikä tekee siitä erinomaisen valinnan TypeScriptillä kehitettyihin sovelluksiin.

## Kuinka:
YAMLin käsittely TypeScriptissä sisältää tyypillisesti YAML-sisällön jäsentämisen JavaScript-objekteiksi ja mahdollisesti JavaScript-objektien muuntamisen takaisin YAMLiksi. Tämä vaatii jäsentimen; yksi suosittu vaihtoehto on `js-yaml`, kirjasto, joka voidaan helposti integroida TypeScript-projekteihin.

### js-yaml:n asentaminen
Ensimmäiseksi, lisää `js-yaml` projektiisi:

```bash
npm install js-yaml
```

### YAMLin jäsentäminen JavaScript-objektiksi
Kuvittele, että sinulla on YAML-tiedosto `config.yaml`, jossa on seuraava sisältö:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Voit lukea ja jäsentää tämän tiedoston JavaScript-objektiksi seuraavasti:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Lataa ja jäsenä YAML-tiedosto
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Esimerkkituloste:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### JavaScript-objektin muuntaminen YAMLiksi
Jos tarvitset tehdä muunnoksen toiseen suuntaan ja muuntaa JavaScript-objektin YAML-merkkijonoksi, voit käyttää `js-yaml`ia seuraavasti:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Esimerkki",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Esimerkkituloste:**

```yaml
title: Esimerkki
is_published: true
author:
  name: Jane Doe
  age: 34
```

Tämä pätkä muuntaa JavaScript-objektin YAML-merkkijonoksi ja tulostaa sen. Käytännössä saatat kirjoittaa tämän takaisin tiedostoon tai käyttää sitä muissa sovelluksesi osissa.
