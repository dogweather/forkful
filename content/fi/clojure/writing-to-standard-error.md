---
title:                "Clojure: Kirjoittaminen standardivirheeseen"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi kirjoittaa standardivirheeseen (standard error) Clojure-ohjelmoinnissa. Yksi tärkeimmistä on virheiden käsittely ja vianjäljitys. Kirjoittamalla standardivirheeseen, voit saada tietoa ohjelman suorituksen aikana tapahtuneista virheistä, mikä helpottaa niiden korjaamista ja estää mahdollisten ongelmien syntymistä.

## Kuinka tehdä se

Standardivirheeseen kirjoittaminen Clojurella on hyvin yksinkertaista. Voit käyttää funktiota `println` ja antaa sille parametrina haluamasi viestin. Esimerkiksi:

```Clojure
(println "Tämä on esimerkki standardivirheen kirjoittamisesta.")
```

Tämä koodi tulostaa viestin "Tämä on esimerkki standardivirheen kirjoittamisesta." standardivirheeseen.

Jos haluat lisätä tietoa virheeseen, voit käyttää funktiota `prn` ja antaa sille parametrina virheen koodin tai arvon. Esimerkiksi:

```Clojure
(prn "Virhekoodi: " 404)
```

Tulostaa "Virhekoodi: 404" standardivirheeseen. Tämä auttaa sinua tunnistamaan ja korjaamaan mahdollisia virheitä koodissasi.

## Syvempi sukellus

Standardivirheeseen kirjoittaminen Clojurella käyttää taustalla Java-luokkaa `System.err`, joka vastaa standardivirheen tulostamisesta. Clojuressa tämä luokkaa voidaan käyttää hyödyntämällä `java.lang.System` -moduulia.

Voit myös asettaa muuttujan `*err*` ja kirjoittaa sen avulla standardivirheeseen. Esimerkiksi:

```Clojure
(set! *err* (java.lang.System/getErr))
(.println *err* "Tämä on esimerkki standardivirheen kirjoittamisesta.")
```

Tämä lähestymistapa saattaa vaikuttaa monimutkaisemmalta, mutta antaa enemmän hallintaa standardivirheeseen kirjoittamisessa. Voit esimerkiksi ohjata standardivirheen tulostuksen toiseen tiedostoon tai käsitellä sitä eri tavalla kuin standarditulostusta.

## Katso myös

- Virheiden käsittely Clojurella: https://clojure.org/guides/errors
- Java-luokka System.err: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html
- Moduuli java.lang.System: https://clojuredocs.org/clojure.core/java.lang/System