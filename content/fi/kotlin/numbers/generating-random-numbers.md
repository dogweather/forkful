---
date: 2024-01-27 20:34:49.745033-07:00
description: "Satunnaislukujen tuottaminen ohjelmoinnissa tarkoittaa sellaisten lukujen\
  \ luomista, joilla ei ole ennustettavaa kaavaa. Ohjelmoijat tekev\xE4t n\xE4in monista\u2026"
lastmod: '2024-03-13T22:44:56.526510-06:00'
model: gpt-4-0125-preview
summary: "Satunnaislukujen tuottaminen ohjelmoinnissa tarkoittaa sellaisten lukujen\
  \ luomista, joilla ei ole ennustettavaa kaavaa. Ohjelmoijat tekev\xE4t n\xE4in monista\u2026"
title: Satunnaislukujen generointi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen tuottaminen ohjelmoinnissa tarkoittaa sellaisten lukujen luomista, joilla ei ole ennustettavaa kaavaa. Ohjelmoijat tekevät näin monista syistä, mukaan lukien simulaatiot, algoritmien testaaminen, pelit ja turvallisuussovellukset, joissa arvaamattomuus on avain realististen tai turvallisten tulosten saavuttamiseen.

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
