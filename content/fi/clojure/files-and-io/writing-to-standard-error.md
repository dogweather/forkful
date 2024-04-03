---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:51.087978-07:00
description: "Kirjoittaminen vakiovirheeseen (stderr) tarkoittaa virheviestien ja\
  \ diagnostiikkatietojen ohjaamista stderr-virtaan, erotettuna vakiotulosteesta (stdout).\u2026"
lastmod: '2024-03-13T22:44:56.200565-06:00'
model: gpt-4-0125-preview
summary: Kirjoittaminen vakiovirheeseen (stderr) tarkoittaa virheviestien ja diagnostiikkatietojen
  ohjaamista stderr-virtaan, erotettuna vakiotulosteesta (stdout).
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Mitä & Miksi?
Kirjoittaminen vakiovirheeseen (stderr) tarkoittaa virheviestien ja diagnostiikkatietojen ohjaamista stderr-virtaan, erotettuna vakiotulosteesta (stdout). Ohjelmoijat tekevät näin erottaakseen tavallisen ohjelman tulosteen virheviesteistä, mikä mahdollistaa tehokkaamman debuggauksen ja lokituksen.

## Kuinka:
Clojuressa voit kirjoittaa stderr-virtaan käyttämällä `*err*`-virtaa. Tässä on perusesimerkki:

```clojure
(.write *err* "Tämä on virheviesti.\n")
```

Huomaa, että viestin kirjoittamisen jälkeen sinun tulisi tyhjentää virta varmistaaksesi, että viesti tulostuu välittömästi:

```clojure
(flush)
```

Esimerkkituloste stderriin:
```
Tämä on virheviesti.
```

Jos käsittelet poikkeuksia, saatat haluta tulostaa pinorakenteet stderriin. Käytä tähän `printStackTrace`:

```clojure
(try
  ;; Koodi, joka saattaa heittää poikkeuksen
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Rakenteellisempaa virhelokitusta varten kolmannen osapuolen kirjastoja, kuten `timbre`, voidaan konfiguroida lokittamaan stderriin. Tässä on perusasetus ja käyttö:

Ensinnäkin, lisää `timbre` riippuvuuksiisi. Sitten konfiguroi se käyttämään stderriä:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Poista käytöstä stdout-lokitus
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Poista käytöstä tiedostolokitus
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Ota käyttöön stderr virheille

(timbre/error "Virhe tapahtui käsitellessäsi pyyntöäsi.")
```

Tämä ohjaa virhetason viestit stderriin, tehden niistä erotettavissa olevia tavanomaisesta sovellustulosteesta.
