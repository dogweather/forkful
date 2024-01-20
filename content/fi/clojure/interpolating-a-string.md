---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpoloivien merkkijonojen käyttö Clojure-ohjelmoinnissa 

## Mikä & Miksi?

Interpoloiva merkkijono on ohjelmointitekniikka, jossa yhdistetään staattinen merkkijono ja dynaamiset arvot yhdeksi uudeksi merkkijonoksi. Ohjelmoijat käyttävät sitä nopeuttamaan ja selkeyttämään koodin kirjoittamista.

## Näin se toimii:

Clojure käyttää `str`-funktiota merkkijonojen yhdistämiseen. Alla on esimerkki:

```Clojure
(let [nimi "Maikki"]
  (str "Hei, " nimi "!"))
```

Merkkijono "`Hei, Maikki!`" palautetaan, koska `nimi`-muuttuja on määritelty arvoksi "Maikki".

## Syvempi tieto:

Interpoloitujen merkkijonojen käyttäminen juontaa juurensa jopa 1960-luvun ohjelmointikielistä. Se on yleinen ominaisuus useimmissa moderneissa ohjelmointikielissä, mukaan lukien Clojure.

Vaihtoehtoisesti Clojure tarjoaa `format`-funktion, joka toimii samankaltaisesti kuin javan `String.format`:

```Clojure
(let [nimi "Maikki" 
      ikä 30]
  (format "Hei, %s! Olet %d vuotta vanha." nimi ikä))
```

Clojuressa, `str` ja `format` -funktiot käsittelevät merkkijono-interpolaatio sen sisällä olevan JVM: n avulla, joten niiden suorituskyky on erittäin hyvä.

## Katso myös:

- Clojure dokumentaatio str funktiolle: https://clojuredocs.org/clojure.core/str
- Clojure dokumentaatio format funktiolle: https://clojuredocs.org/clojure.core/format
- Lisää tietoja merkkijonojen interpoloinnista: https://clojure.org/guides/learn/functions