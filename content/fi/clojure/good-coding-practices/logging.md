---
date: 2024-01-26 01:02:09.603902-07:00
description: "Kuinka tehd\xE4: Clojure nojautuu Javan lokitustoimintoihin, mutta voit\
  \ ottaa ne k\xE4ytt\xF6\xF6n idiomaattisemmin Clojuren tapaan. Katsotaanpa, miten\
  \ voit k\xE4ytt\xE4\xE4\u2026"
lastmod: '2024-03-13T22:44:56.190608-06:00'
model: gpt-4-1106-preview
summary: "Clojure nojautuu Javan lokitustoimintoihin, mutta voit ottaa ne k\xE4ytt\xF6\
  \xF6n idiomaattisemmin Clojuren tapaan."
title: Lokitus
weight: 17
---

## Kuinka tehdä:
Clojure nojautuu Javan lokitustoimintoihin, mutta voit ottaa ne käyttöön idiomaattisemmin Clojuren tapaan. Katsotaanpa, miten voit käyttää `clojure.tools.logging`-kirjastoa, joka tarjoaa yksinkertaisen abstraktion usean lokituskehyksen päälle:

Ensiksi, lisää riippuvuus `clojure.tools.logging` ja jokin lokitusimplementaatio kuten `log4j` `project.clj`-tiedostoosi:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Nyt, lokitetaan joitakin viestejä:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Aloitetaan intensiivinen laskenta...")
  (Thread/sleep 3000) ; Simuloidaan pitkä laskenta
  (log/info "Laskenta valmis. Vastaus on 42.")
  42)

(compute-answer-to-everything)
```
Lokituloste ei oletuksena näytä `DEBUG`-viestejä, koska lokitasot on tyypillisesti asetettu tasolle `INFO`:

```
INFO  [sinun-nimesiavaruutesi] - Laskenta valmis. Vastaus on 42.
```

Voit konfiguroida lokitason ja -lähteet `log4j.properties`-tiedostossa saadaksesi tarvittaessa yksityiskohtaisempaa lokitietoa.

## Syväsukellus
Clojuren `clojure.tools.logging` on ollut olemassa jo jonkin aikaa ja toimii siltaa Clojure-koodin ja Javan lokitusmaailman välillä. Historiallisesti Java on käynyt läpi useita iteraatioita ja kirjastoja lokituksen osalta, kuten Javan sisäänrakennettu lokitus API, `log4j`, `slf4j` ja `logback`.

Clojuressa, vaikka voit suoraan käyttää Javan lokituskehyksiä, `clojure.tools.logging` tunnistaa ja delegoi sille lokituskehykselle, jonka se löytää luokkapolkustasi, säästäen sinut tiukasta sitoutumisesta tiettyyn toteutukseen. Tämä voi auttaa pitämään Clojure-koodisi portattavampana ja modulaarisempana.

Vaihtoehtoja `clojure.tools.logging`-kirjastolle Clojuren ekosysteemissä on kirjastoja kuten `timbre`, joka on puhdas Clojure-lokituskirjasto ominaisuuksilla kuten lokien kierrätys, suodattaminen ja asynkroninen lokitus valmiina.

Toteutuksen yksityiskohdat ovat ratkaisevia lokituksessa, kun työskennellään monisäikeisessä ympäristössä kuten Clojure. Tässä, muuttumattomuus ja sivuvaikutusten hallinta tarjoavat selkeitä etuja. Lokitus, sivuvaikutuksena, tulisi käsitellä huolellisesti, jotta vältetään suorituskyvyn pullonkaulat ja taataan säieturvallisuus, jotka useimmat Javan lokituskehykset jo huolehtivat.

Lopuksi, harkitse rakenteellista lokitusta, missä lokit kirjoitetaan rakenteellisena datana (kuten JSON). Tämä voi olla erittäin hyödyllistä myöhemmässä analysoinnissa ja prosessoinnissa, erityisesti kun käsitellään suuren mittakaavan hajautettuja järjestelmiä.

## Katso myös
Jos haluat lisää tietoa, harkitse näiden resurssien tutkimista:

- Clojure Tools Logging dokumentaatio: https://github.com/clojure/tools.logging
- Timbre, Clojure-lokituskirjasto: https://github.com/ptaoussanis/timbre
- Log4J:n konfigurointi Clojuressa: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Logback-käsikirja edistyneisiin asetuksiin: http://logback.qos.ch/manual/
- Opas rakenteellisesta lokituksesta Clojuressa: https://corfield.org/blog/2020/04/28/structured-logging/
