---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:10.989993-07:00
description: "Kuinka: Clojurena, ollessaan JVM-kieli, voi hy\xF6dynt\xE4\xE4 Javan\
  \ `java.io.File` -luokkaa t\xE4h\xE4n tarkoitukseen. Et tarvitse mit\xE4\xE4n kolmannen\
  \ osapuolen kirjastoa\u2026"
lastmod: '2024-03-13T22:44:56.198665-06:00'
model: gpt-4-0125-preview
summary: "Clojurena, ollessaan JVM-kieli, voi hy\xF6dynt\xE4\xE4 Javan `java.io.File`\
  \ -luokkaa t\xE4h\xE4n tarkoitukseen."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka:
Clojurena, ollessaan JVM-kieli, voi hyödyntää Javan `java.io.File` -luokkaa tähän tarkoitukseen. Et tarvitse mitään kolmannen osapuolen kirjastoa tällaiseen perustoimintoon. Tässä on, miten voit tehdä sen:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; Käyttöesimerkki
(println (directory-exists? "/polku/tiedostohakemistoosi")) ;; true tai false
```

Tämä funktio, `directory-exists?`, ottaa hakemistopolun merkkijonona ja palauttaa `true`, jos hakemisto on olemassa, ja `false` muuten. Tämä saavutetaan luomalla `File` -objekti hakemistopolulla ja sitten kutsumalla `.exists` -metodia tähän objektiin.

Lisäksi raakaan Java-yhteistyöhön voit käyttää Clojure-kirjastoja, jotka abstrahoivat joitakin Javan boilerplate-koodia. Yksi tällainen kirjasto on `clojure.java.io`. Kuitenkin, hakemiston olemassaolon tarkistamiseksi käyttäisit silti `File` -luokkaa, mutta saatat pitää kirjastoa hyödyllisenä muissa tiedosto-operaatioissa. Esimerkki:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; Esimerkkikäyttö
(println (directory-exists?-clojure "/toinen/polku/tarkistettavaksi")) ;; true tai false
```

Tämä versio on melko samanlainen, mutta käyttää Clojuren `io/file`-funktiota `File`-objektin luomiseen. Tämä menetelmä sulautuu luontevammin Clojure-koodikantoihin hyödyntämällä Clojuren IO-operaatioiden kirjastoa, sen sijaan, että se suoraan käyttäisi Javan luokkia.
