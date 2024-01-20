---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen luonti on ohjelmointitoimintaa, jossa luodaan ennalta-arvaamattomia numeroita. Ohjelmoijat tekevät sitä, koska se on välttämätöntä sovelluksille, jotka tarvitsevat ainutlaatuisuutta tai joiden on simuloitava aitoa satunnaisuutta, kuten pelit ja tietoturvaratkaisut.

## Kuinka:

Tässä on esimerkkikoodi satunnaisluvun luomiseksi Kotlinissa:

```Kotlin
   import kotlin.random.Random

   fun main() {
       val random = Random.nextInt(0, 100)
       println("Satunnaisluku on $random!")
   }
```

Koodipalanen luo satunnaisen kokonaisluvun välillä 0-100 ja tulostaa sen. Esimerkkiajossa se saattaa tulostaa jotakin tämänkaltaista:

```
Satunnaisluku on 42!
```

## Syvempi Sukellus:

Satunnaislukujen luonnin historiallinen konteksti juontaa juurensa matematiikkaan ja luo perustan monille nykyaikaisille digitaaliteknologioille. Satunnaislukujen generaattoreita on monia erilaisia, joilla on erilaiset hyödyt ja haitat: esimerkiksi pseudosatunnaislukugeneraattorit (PRNG) tuottavat toistettavan sarjan, kun taas todelliset satunnaislukugeneraattorit (TRNG) tuottavat aidosti satunnaisia lukuja.

Kotlinissa satunnaislukujen luominen tapahtuu `Random`-luokan kautta. Tämä luokka tarjoaa paljon erilaisia metodeja erityyppisten satunnaislukujen luomiseen, kuten `nextInt`, `nextDouble`, `nextBytes` jne.

## Katso Myös:

- Kotlinin virallinen dokumentaatio Random-luokasta: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/
- Vakituisempi keskustelu satunnaislukugeneraattoreista ja niiden historiasta: https://encyclopediaofmath.org/wiki/Random_number_generator
- Googlen artikkeli satunnaisluvun käyttämisestä peleissä: https://developers.google.com/games/services/common/concepts/realtimeMultiplayer