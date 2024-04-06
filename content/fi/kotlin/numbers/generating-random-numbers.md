---
date: 2024-01-27 20:34:49.745033-07:00
description: "Kuinka: Kotlin tarjoaa suoraviivaisen tavan satunnaislukujen tuottamiseen\
  \ sen vakio kirjaston kautta. N\xE4in voit tuottaa erityyppisi\xE4 satunnaisia arvoja."
lastmod: '2024-04-05T21:53:58.093616-06:00'
model: gpt-4-0125-preview
summary: Kotlin tarjoaa suoraviivaisen tavan satunnaislukujen tuottamiseen sen vakio
  kirjaston kautta.
title: Satunnaislukujen generointi
weight: 12
---

## Kuinka:
Kotlin tarjoaa suoraviivaisen tavan satunnaislukujen tuottamiseen sen vakio kirjaston kautta. Näin voit tuottaa erityyppisiä satunnaisia arvoja:

### Satunnaisen kokonaisluvun tuottaminen
Satunnaisen kokonaisluvun tuottaminen tietyllä välillä:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Tuottaa satunnaisen luvun välillä 1 ja 99
    println(randomNumber)
}
```

### Satunnaisen liukuluvun tuottaminen
Samoin satunnaisen liukuluvun tuottaminen:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Tuottaa satunnaisen liukuluvun välillä 1.0 ja 10.0
    println(randomDouble)
}
```

### Satunnaisen totuusarvon tuottaminen
Satunnaisen totuusarvon tuottamiseksi:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Tuottaa satunnaisesti joko tosi tai epätosi
    println(randomBoolean)
}
```

### Kylvöarvo toistettavien tulosten saamiseksi
Tapauksissa, joissa tarvitset toistettavia satunnaislukujen sekvenssejä (esimerkiksi testauksessa), voit kylvää satunnaislukugeneraattorin:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Syväsukellus
Kotlinin vakio kirjaston lähestymistapa satunnaislukujen tuottamiseen hyödyntää Java'n `java.util.Random`ia taustalla, varmistaen käyttöhelppouden ja suorituskyvyn sekoituksen. On kuitenkin tärkeää huomata, että nämä menetelmät tuottavat pseudosatunnaislukuja, mikä tarkoittaa, että luvut vaikuttavat satunnaisilta, mutta ne on tuotettu deterministisellä prosessilla.

Useimmissa sovelluksissa Kotlinin `Random` luokan tarjoama satunnaisuus on riittävä. Kuitenkin, turvallisuus-herkillä sovelluksilla, kuten kryptografiassa, jossa satunnaisuuden laatu on ensiarvoisen tärkeää, tulisi harkita `java.security.SecureRandom` käyttämistä sen sijaan. SecureRandom on erityisesti suunniteltu kryptografisiin toimenpiteisiin, tarjoten korkeamman laadun satunnaisuutta, vaikkakin mahdollisesti suorituskyvyn kompromissilla.

Kotlin ei keksi pyörää uudelleen, mutta tarjoaa Kotlin-ystävällisen API:n Javan satunnaislukujen tuottamismekanismien päälle, tehdäkseen sen idiomaattisemmaksi ja tiiviimmäksi käyttää Kotlin-projekteissa. Kuten aina satunnaisuutta käsiteltäessä, ohjelmoijien tulisi huolellisesti harkita käyttötapaus valitakseen työkalun, joka sopii parhaiten tehtävään.
