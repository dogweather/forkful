---
title:    "Kotlin: Tekstitiedoston lukeminen."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi lukua tekstitiedostoon on tärkeä taito ohjelmointimaailmassa. Se voi auttaa sinua käsittelemään suuria tietomääriä, tallentamaan ja lukemaan käyttäjän syötteitä, tai yksinkertaisesti helpottamaan tiedon tallentamista ja jakamista ohjelman kanssa.

## Miten

Aloittamiseen, sinun täytyy avata käyttämäsi tiedoston lukija käyttämällä `BufferedReader` luokkaa Kotlinissa. Sitten voit käyttää `useLines` funktiota lukeaksesi tiedoston sisältöä rivillä kerrallaan. 

```Kotlin
val tiedostonLukija = BufferedReader(FileReader(path))
tiedostonLukija.useLines { rivit ->
    rivit.forEach { rivi ->
        // tee jotain rivin kanssa
    }
}
```

Jos haluat vain lukea koko tiedoston sisällön kerralla, voit käyttää `readLines` funktiota, joka palauttaa listan kaikista tiedoston riveistä. 

```Kotlin
val tiedostonSisalto = File(path).readLines()
println(tiedostonSisalto)
```

Voit myös luoda oman `File` olion ja käyttää `reader` funktion antamaan `BufferedReader` olion, jolloin sinun ei tarvitse erikseen avata tai sulkea tiedostoa.

```Kotlin
val tiedosto = File(path)
val rivit = tiedosto.reader().readLines()
println(rivit)
```

## Syventävä tarkastelu

Tekstitiedostojen lukeminen Kotlinissa on todella yksinkertaista ja helppoa, mutta on myös muutama asia, jotka on hyvä pitää mielessä. Ensinnäkin, muista aina käsitellä tiedoston lukuun liittyvät poikkeukset, kuten jos tiedostoa ei löydy tai sitä ei voida avata. 

Toiseksi, voit käyttää `forEachLine` funktiota `useLines` sijasta, jos haluat käsitellä jokaista riviä erikseen sen sijaan, että luot listaa kaikista tiedoston riveistä. 

Lopuksi, tiedostot voivat olla eri koodaustyyleillä (esim. UTF-8, ISO-8859-1) joten varmista, että käytät oikeaa koodaustyyppiä lukiessasi tekstitiedostoa.

## Katso myös

- Kotlinin `BufferedReader` dokumentaatio: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/index.html
- Esimerkkikoodi tekstitiedostojen lukemisesta Kotlinissa: https://kotlinlang.org/docs/tutorials/kotlin-for-py/read-files.html
- Tiedostonluku käyttäen `File` luokkaa: https://kotlinlang.org/docs/tutorials/kotlin-for-py/read-files-advanced.html