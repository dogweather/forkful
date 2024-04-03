---
date: 2024-01-26 01:08:50.605300-07:00
description: "Lokitus ohjelmoinnissa on kuin p\xE4iv\xE4kirjan pit\xE4mist\xE4 sovelluksellesi.\
  \ Se on systemaattista tapahtumien, viestien ja datapisteiden tallentamista, jotka\u2026"
lastmod: '2024-03-13T22:44:57.093411-06:00'
model: gpt-4-1106-preview
summary: "Lokitus ohjelmoinnissa on kuin p\xE4iv\xE4kirjan pit\xE4mist\xE4 sovelluksellesi."
title: Lokitus
weight: 17
---

## Miten:
Ruby sisältää sisäänrakennetun moduulin lokitukseen, `Logger`, joka on superhelppo käyttää. Tässä on nopea esimerkki, jolla pääset alkuun:

```ruby
require 'logger'

# Luo Logger, joka tulostaa STDOUT:iin
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Esimerkki lokiviesteistä
logger.info("Tämä on infotason viesti")
logger.warn("Tämä on varoitustason viesti")
logger.error("Tämä on virhetason viesti")
```

Yllä olevan skriptin suorittaminen tulostaa jotain tällaista:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : Tämä on infotason viesti
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : Tämä on varoitustason viesti
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : Tämä on virhetason viesti
```

Voit määrittää lokin muodon ja tason tarpeettoman kohinan suodattamiseksi, ja voit ohjata lokiinpanoja eri kohteisiin, kuten tiedostoon tai jopa ulkoiseen lokituspalveluun.

## Syväsukellus
Lokitus on kuin ikivanha perinne ohjelmoinnissa. Historiallisesti lokit olivat yksinkertaisia tekstiedostoja, joita käsiteltiin manuaalisesti työkaluilla kuten `grep`. Mutta käsite laajeni koko ekosysteemiksi, joka käsittää vankkoja lokituskehyksiä ja -palveluita kuten Log4j, Syslog Linuxilla tai Sematext ja Loggly pilviaikakaudella.

Rubyn `Logger` on vaivaton tapa päästä alkuun, mutta jos tarvitset enemmän tehoa ja joustavuutta, saatat haluta tutkia vaihtoehtoja kuten Lograge tai Semantic Logger. Nämä kirjastot toimivat hyvin Ruby-sovellusten kanssa, tarjoten tarkempaa hallintaa logien muotoiluun, mukaan lukien rakenteelliset lokit (JSON-muoto), parempaa suorituskykyä ja saumattoman integraation muiden palveluiden kanssa.

Jokaisella Ruby-lokikirjastolla on omat tapansa tehdä asioita, mutta syvällä sisällä ne kaikki pyörivät logger-instanssin ympärillä, jolle lähetät viestejä. Logger käsittelee näitä viestejä asetettujen tasojen perusteella – DEBUG, INFO, WARN, ERROR, FATAL ja UNKNOWN – ja päättää, mitä niille tehdään: tulostetaanko ne, tallennetaanko tiedostoon, lähetetäänkö verkon yli jne.

## Katso myös
Syväsukellus Rubyn sisäänrakennettuun lokitusmoduuliin, tutustu virallisiin dokumentteihin:

Jos olet kiinnostunut edistyneemmästä lokituksesta tai haluat tutkia kolmannen osapuolen jalokiviä:
- [Lograge](https://github.com/roidrage/lograge)

Yleiset lokituskäytännöt ja -filosofia (ei Ruby-spesifisiä), nämä artikkelit ovat ajattomia lukemistoja:
- [Googlen sivuston luotettavuutta käsittelevän kirjan luku 16: Kuormituksen käsittely](https://sre.google/sre-book/handling-overload/#log-messages)
- [12 tekijän sovellus - Lokit](https://12factor.net/logs)
