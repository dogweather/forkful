---
title:                "Työskentely YAML:n kanssa"
aliases:
- fi/bash/working-with-yaml.md
date:                  2024-02-03T19:24:48.391143-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

YAML, joka tarkoittaa YAML Ain't Markup Languagea, on ihmisluettava tiedon serialisointistandardi, jota voidaan käyttää asetustiedostoissa sekä sovelluksissa, joissa dataa tallennetaan tai välitetään. Ohjelmoijat suosivat YAMLia sen selkeyden ja yksinkertaisuuden vuoksi, erityisesti projekteissa, jotka sisältävät monimutkaisia kokoonpanoja tai tarvitsevat helposti muokattavia tietorakenteita.

## Miten:

Suoraan YAML:n kanssa työskentely Bashissa vaatii hieman kekseliäisyyttä, koska Bash ei sisällä valmiiksi tukia YAML:n jäsentämiseen. Voit kuitenkin käyttää ulkoisia työkaluja, kuten `yq`:ta (kevyt ja siirrettävä komentorivin YAML-prosessori), interaktiivisesti työskennelläksesi YAML-tiedostojen kanssa tehokkaasti. Käydään läpi joitakin yleisiä toimenpiteitä:

### `yq`:n asentaminen:

Ennen esimerkkien käsittelyä, varmista että sinulla on `yq` asennettuna. Sen voi yleensä asentaa paketinhallintasi kautta, esimerkiksi Ubuntussa:

```bash
sudo apt-get install yq
```

Tai voit ladata sen suoraan sen GitHub-repositoriosta.

### Arvon lukeminen:

Oletetaan, että sinulla on tiedosto nimeltä `config.yaml`, jossa on seuraava sisältö:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

Tietokannan isäntää voit lukea käyttäen `yq` seuraavasti:

```bash
yq e '.database.host' config.yaml
```

**Esimerkkituloste:**

```
localhost
```

### Arvon päivittäminen:

Käyttäjän nimen päivittämiseen `config.yaml`:ssa, käytä `yq eval` komentoa `-i` (paikallaan) valitsimen kanssa:

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Varmista muutos käyttäen:

```bash
yq e '.user.name' config.yaml
```

**Esimerkkituloste:**

```
newadmin
```

### Uuden elementin lisääminen:

Lisätäksesi uuden elementin tietokanta-osioon, kuten uusi kenttä `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Tiedoston sisällön tarkistaminen vahvistaa lisäyksen.

### Elementin poistaminen:

Poistaaksesi salasanan käyttäjän alta:

```bash
yq e 'del(.user.password)' -i config.yaml
```

Tämä toimenpide poistaa salasana-kentän konfiguraatiosta.

Muista, että `yq` on tehokas työkalu ja sillä on paljon muitakin kyvykkyyksiä, mukaan lukien YAML:n muuntaminen JSON:ksi, tiedostojen yhdistäminen ja vielä monimutkaisemmat manipulaatiot. Tutustu `yq` dokumentaatioon lisätutkimuksia varten.
