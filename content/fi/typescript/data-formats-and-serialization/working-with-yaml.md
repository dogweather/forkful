---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:18.898952-07:00
description: "YAML, ihmisl\xE4heisesti suunniteltu tietojen serialisointikieli, on\
  \ usein k\xE4yt\xF6ss\xE4 konfiguraatiotiedostoissa, prosessien v\xE4lisess\xE4\
  \ viestinn\xE4ss\xE4 ja tietojen\u2026"
lastmod: '2024-03-13T22:44:56.335129-06:00'
model: gpt-4-0125-preview
summary: "YAML, ihmisl\xE4heisesti suunniteltu tietojen serialisointikieli, on usein\
  \ k\xE4yt\xF6ss\xE4 konfiguraatiotiedostoissa, prosessien v\xE4lisess\xE4 viestinn\xE4\
  ss\xE4 ja tietojen tallennuksessa."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

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
