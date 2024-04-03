---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:02.466354-07:00
description: "Kuinka: Clojure, ollessaan JVM-kieli, mahdollistaa Java String -metodien\
  \ suoran k\xE4yt\xF6n. T\xE4ss\xE4 on perusesimerkki kuinka muuttaa merkkijonon\
  \ alkukirjaimen\u2026"
lastmod: '2024-03-13T22:44:56.168640-06:00'
model: gpt-4-0125-preview
summary: "Clojure, ollessaan JVM-kieli, mahdollistaa Java String -metodien suoran\
  \ k\xE4yt\xF6n."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Kuinka:
Clojure, ollessaan JVM-kieli, mahdollistaa Java String -metodien suoran käytön. Tässä on perusesimerkki kuinka muuttaa merkkijonon alkukirjaimen isoksi Clojurella:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure ei sisällä sisäänrakennettua funktiota nimenomaan merkkijonojen alkukirjaimen muuttamiseen isoksi, mutta kuten näytetty, tämän voi helposti saavuttaa yhdistämällä `clojure.string/upper-case`, `subs`, ja `str` funktiot.

Monimutkaisempien merkkijonojen käsittelyä varten saatat kääntyä kolmannen osapuolen kirjaston puoleen. Yksi tällainen suosittu kirjasto Clojure-ekosysteemissä on `clojure.string`. Kuitenkaan, viimeisimmän päivitykseni mukaan, se ei tarjoa suoraa `capitalize` funktiota yli sen, mitä on esitetty perus Clojure-toiminnallisuuksilla, joten yllä näytetty menetelmä on suoraviivainen lähestymistapa ilman, että tarvitsee vetää mukaan lisäkirjastoja pelkästään alkukirjaimen muuttamista varten.

Muista, kun työskentelet Clojuressa merkkijonojen kanssa, jotka vuorovaikuttavat Java-metodien kanssa, käytät käytännössä Java-merkkijonoja. Tämä mahdollistaa sinun hyödyntää koko Java String -metodien arsenaalia suoraan Clojure-koodissasi tarvittaessa.
