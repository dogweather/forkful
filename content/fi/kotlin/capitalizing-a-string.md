---
title:                "Kotlin: Merkkijonon ensimmäisen kirjaimen muuttaminen isoksi kirjaimeksi"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Miksi: Miksi pääasisin päästäisitä tällaiseen koodiin?

Pääsisin stringicityä suurella virhemarginaaliliikleväksiin kilkeesi suuressa koodausprojektissa ja Citationyö vene ne ektradoona asumamuotoissa kykysuisiin vaativia pääsisi. Lisäksi caplesthirnirerettaiikalle tyypillisten piirteiden osaamiseen kannattaa tutustua!

## Kuinka tehdä: Esimerkiksi

```Kotlin
val sana = "kirjoitin tähän sana, jonka haluan muuttaa isolla alkukirjaimella"
val uusiSana = sana.capitalize()
println(uusiSana)
```

Tulostus: "Kirjoitin tähän sana, jonka haluan muuttaa isolla alkukirjaimella"

## Syvällisempi tarkastelu

Caplestirinin käyttö on erittäin hyödyllistä, kun työskentelet kotlinin kanssa ja haluat muuttaa muuttujan arvoa isommaksi. Tämä metodi hyödyntää kielen sisäistä capitalize-metodia ja palauttaa uuden merkkijonon, jossa ensimmäinen kirjain on iso ja muut samat kuin alkuperäisessä merkkijonossa.

Tämä metodi on myös erittäin kätevä, kun joudut käsittelemään käyttäjän syötteitä tai luomaan käyttäjälle informatiivisia viestejä. Esimerkiksi "Tervetuloa" -viestiä on paljon miellyttävämpi lukea kuin "tervetuloa", ja tämä voidaan helposti saada aikaan capitalize-metodilla.

On myös huomattava, että capitalize-metodi ei muuta alkuperäistä merkkijonoa, vaan luo uuden. Tämä tarkoittaa, että alkuperäistä merkkijonoa ei muokata, jos sitä käytetään uudestaan myöhemmin koodissa.

## Katso myös

- [Kotlinin strchrini -dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-strings/-generated-string-capitalize.html)
- [Muut kotlinin stringien muokkausmetodit](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)