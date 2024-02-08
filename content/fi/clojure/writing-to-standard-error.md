---
title:                "Kirjoittaminen standardivirheeseen"
aliases:
- fi/clojure/writing-to-standard-error.md
date:                  2024-02-03T19:32:51.087978-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
