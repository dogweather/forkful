---
date: 2024-01-20 17:45:32.720898-07:00
description: "Merkkijonojen poimiminen tarkoittaa osan ottamista isommasta merkkijonosta.\
  \ Ohjelmoijat tekev\xE4t n\xE4in datan muokkaamiseksi tai hy\xF6dyllisen informaation\u2026"
lastmod: '2024-03-13T22:44:56.173813-06:00'
model: gpt-4-1106-preview
summary: "Merkkijonojen poimiminen tarkoittaa osan ottamista isommasta merkkijonosta.\
  \ Ohjelmoijat tekev\xE4t n\xE4in datan muokkaamiseksi tai hy\xF6dyllisen informaation\u2026"
title: Merkkijonojen osien poimiminen
---

{{< edit_this_page >}}

## What & Why?
Merkkijonojen poimiminen tarkoittaa osan ottamista isommasta merkkijonosta. Ohjelmoijat tekevät näin datan muokkaamiseksi tai hyödyllisen informaation irrottamiseksi.

## How to:
Clojuren `subs` funktio tekee tämän homman mutkattomasti.

```Clojure
(defn demo-substrings []
  (let [text "Hello from Finland!"]
    (println (subs text 6 10))    ; Tulostaa "from"
    (println (subs text 11))      ; Tulostaa "Finland!"
    ))

(demo-substrings)
```

Tuloste:

```
from
Finland!
```

## Deep Dive
Substringien poiminta ei ole uusi juttu; se on peräisin ajoista, jolloin ihmiset alkoivat käsitellä tietokoneilla tekstejä. Clojure käyttää Javan `substring` funktiota tämän toiminnallisuuden toteuttamisessa, koska se on Javan virtuaalikoneella toteutettu kieli.

Vaihtoehtoisesti, voi käyttää myös `take` ja `drop` funktioita yhdessä `apply str` kanssa osajonon ottamiseen. Tämä on kätevä, jos halutaan käyttää merkkijonoja kokoelmien tavoin.

Toteutusyksityiskohtina, `subs` toimii indekseillä, jotka kertovat mistä kohtaa poimia merkkijono. Hyvä tietää: Indeksit alkavat aina nollasta.

## See Also
Lisää tietoa ja esimerkkejä:
- Clojure-dokumentaatio `subs`: [https://clojuredocs.org/clojure.core/subs](https://clojuredocs.org/clojure.core/subs)
- Clojuren `take` ja `drop` funktiot: [https://clojuredocs.org/clojure.core/take](https://clojuredocs.org/clojure.core/take), [https://clojuredocs.org/clojure.core/drop](https://clojuredocs.org/clojure.core/drop)
