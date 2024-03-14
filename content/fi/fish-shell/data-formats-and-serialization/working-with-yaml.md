---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:31.388613-07:00
description: "YAML-tiedostojen k\xE4sittelyyn kuuluu YAML-tiedostojen (YAML Ain't\
  \ Markup Language) j\xE4sent\xE4minen ja manipulointi, jotka ovat konfiguraatiotiedostoissa\u2026"
lastmod: '2024-03-13T22:44:57.015363-06:00'
model: gpt-4-0125-preview
summary: "YAML-tiedostojen k\xE4sittelyyn kuuluu YAML-tiedostojen (YAML Ain't Markup\
  \ Language) j\xE4sent\xE4minen ja manipulointi, jotka ovat konfiguraatiotiedostoissa\u2026"
title: "Ty\xF6skentely YAML:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?
YAML-tiedostojen käsittelyyn kuuluu YAML-tiedostojen (YAML Ain't Markup Language) jäsentäminen ja manipulointi, jotka ovat konfiguraatiotiedostoissa käytettyä datan sarjallistamismuotoa, Fish Shellissä. Ohjelmoijat tekevät tätä automatisoidakseen ja konfiguroidakseen sovelluksia tai palveluita tehokkaasti shell-ympäristöjen kontekstissa, helpottaen tehtäviä kuten konfiguraatioiden hallinta ja resurssien varaus.

## Kuinka:
Fish Shellillä ei ole sisäänrakennettua tukea YAML:n jäsentämiseen, mutta voit käyttää kolmannen osapuolen työkaluja, kuten `yq` (kevyt ja kannettava komentorivin YAML-prosessori) käsittelemään YAML-dataa.

**yq:n asennus (jos ei vielä asennettu):**
```fish
sudo apt-get install yq
```

**Arvon lukeminen YAML-tiedostosta:**
Oletetaan, että sinulla on `config.yaml` YAML-tiedosto seuraavalla sisällöllä:
```yaml
database:
  host: localhost
  port: 3306
```

Lukeaksesi tietokannan hostin, käyttäisit:
```fish
set host (yq e '.database.host' config.yaml)
echo $host
```
**Esimerkkituloste:**
```
localhost
```

**Arvon päivittäminen YAML-tiedostossa:**
Päivittääksesi `port` arvon `5432`ksi, käytä:
```fish
yq e '.database.port = 5432' -i config.yaml
```
**Vahvista päivitys:**
```fish
yq e '.database.port' config.yaml
```
**Esimerkkituloste:**
```
5432
```

**Uuden YAML-tiedoston kirjoittaminen:**
Luodaksesi uuden `new_config.yaml` määritellyllä sisällöllä:
```fish
echo "webserver:
  host: '127.0.0.1'
  port: 8080" | yq e -P - > new_config.yaml
```
Tämä käyttää `yq`:ta käsittelemään ja kauniisti tulostamaan (-P lippu) merkkijonon uuteen YAML-tiedostoon.

**Monimutkaisten rakenteiden jäsentäminen:**
Jos sinulla on monimutkaisempi YAML-tiedosto ja tarvitset haettavan sisäkkäisiä taulukoita tai objekteja, voit:
```fish
echo "servers:
  - name: server1
    ip: 192.168.1.101
  - name: server2
    ip: 192.168.1.102" > servers.yaml

yq e '.servers[].name' servers.yaml
```
**Esimerkkituloste:**
```
server1
server2
```
`yq`:n avulla Fish Shell tekee YAML-dokumenttien halki navigoinnista ja niiden manipuloinnista suoraviivaista erilaisia automaatio- ja konfiguraatiotehtäviä varten.
