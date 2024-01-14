---
title:    "Kotlin: Sattumanvaraisten lukujen luominen"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Miksi: Miksi satunnaislukujen generoiminen on tärkeää?

Satunnaislukujen generoiminen on tärkeä osa monia ohjelmointitehtäviä, kuten pelien luomista, tietokoneiden simuloimista ja salausavainten luomista. Satunnaiset luvut ovat tarpeen myös testauksessa ja data-analyysissä. Joten olipa sitten ammattilainen tai harrastelija, satunnaislukujen generoiminen on välttämätöntä monissa ohjelmoinnin alan tehtävissä.

# Miten: Satunnaislukujen generoiminen Kotlinilla

## Perussatunnaislukujen generointi
Kotlin tarjoaa valmiin random-luokan, jolla voi generoida satunnaisia lukuja. Se käyttää sisäistä satunnaislukugeneraattoria, joka perustuu [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)-algoritmiin. Voit luoda uuden random-olion viittaamalla luokkaan "Random()" ja käyttää sen "nextInt()" tai "nextDouble()" metodia halutun satunnaisen numeron saamiseksi. Esimerkiksi:

```Kotlin
val random = Random()
val randomNumber = random.nextInt(10) //generoi satunnaisen kokonaisluvun väliltä 0-9
```

## Satunnaisen merkkijonon generointi
Satunnaisen merkkijonon generoiminen voi olla hyödyllistä esimerkiksi testauksessa tai tunnisteiden luomisessa. Tähän voit käyttää Kotlinin "randomUUID()" metodia, joka generoi satunnaisen universaalisti uniikin tunnisteen. Esimerkiksi:

```Kotlin
val randomUUID = UUID.randomUUID().toString() //generoi satunnaisen UUID-merkkijonon
```

# Syvempi sukellus: Satunnaislukujen generoinnin taustalla

Kuten mainittu, Kotlinin random-luokka käyttää Mersenne Twister -algoritmia satunnaislukujen generoimiseen. Tämä on yksi suosituimmista satunnaislukugeneraattoreista, ja se on suunniteltu tuottamaan laadukkaita satunnaislukuja, joissa on pitkä jakso toistamattomia lukuja. Satunnaislukugeneraattorin tehokkuuden ja laadun ylläpitämiseksi se ottaa sisäisesti käyttöön siemenluvun, jota se muuttaa jokaisen satunnaisen luvun generoinnin jälkeen.

On myös hyvä huomata, että satunnaislukujen generoiminen on pseudosatunnaisen prosessi, mikä tarkoittaa, että siemenluvun mukaisella algoritmilla generoidut luvut eivät ole täysin satunnaisia. Siitä huolimatta Mersenne Twister -algoritmi tuottaa erittäin laadukkaita lukuja, joita voidaan pitää riittävänä monissa ohjelmointitehtävissä.

# Katso myös:

- [Kotlinin virallinen dokumentaatio satunnaisluvuista](https://kotlinlang.org/docs/reference/basic-types.html#random-numbers)
- [Mersenne Twister -algortimin perusteet](https://en.wikipedia.org/wiki/Mersenne_Twister)