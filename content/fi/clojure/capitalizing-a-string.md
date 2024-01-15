---
title:                "Merkkijonon isojen kirjainten muokkaaminen"
html_title:           "Clojure: Merkkijonon isojen kirjainten muokkaaminen"
simple_title:         "Merkkijonon isojen kirjainten muokkaaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon alkukirjaimen isoksi? Se voi olla hyödyllistä esimerkiksi silloin, kun tarvitaan yhdenmukainen muotoilu tai vertailu.

## Miten tehdä se

Capitalization eli alkukirjaimen suurentaminen voidaan hoitaa helposti Clojurella `capitalize`-funktiolla. Siirry `user`-namespacen sisälle ja kokeile seuraavaa:

```Clojure
(capitalize "tämä on merkkijono")
```

Tulos tulisi näyttää tältä:

```Clojure
"Tämä on merkkijono"
```

Jos halutaan suurentaa vain merkkijonon ensimmäinen kirjain, käytetään `capitalize-first`-funktiota:

```Clojure
(capitalize-first "eka sana on iso")
```

Tuloksen pitäisi olla:

```Clojure
"Eka sana on iso"
```

## Syvempi sukellus

Clojuren `capitalize`-funktio käyttää Java-luokkien `Character`-luokkaa, joka tarjoaa monia eri metodeja merkkien käsittelyyn.

JAVA API:n mukaan `capitalize`-metodi muuttaa kaikki merkin lukuasettotunnisteen (`islowert`-metodi) palauttamat merkit ison kirjaimen muotoon. Tämä tarkoittaa, että esimerkiksi skandinaaviset kirjaimet, kuten "å" tai "ö", eivät välttämättä muutu oikeaksi isoksi kirjaimeksi.

Jos haluat taata, että kaikki merkit muutetaan isoksi, voit käyttää `toUpperCase`-metodia:

```Clojure
(.toUpperCase "änkkäin muuttuu isoksi")
```

Tuloksen pitäisi olla:

```Clojure
"ÄNKKÄIN MUUTTUU ISOKSI"
```

## Katso myös

- [Clojure Dokumentaatio - capitalize](https://clojuredocs.org/clojure.core/capitalize)
- [Java API - Character-luokka](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html)