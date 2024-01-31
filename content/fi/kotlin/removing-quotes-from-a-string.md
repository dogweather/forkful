---
title:                "Merkkijonosta lainausmerkkien poistaminen"
date:                  2024-01-26T03:41:42.186291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lainausmerkkien poistaminen merkkijonosta tarkoittaa kaikkien lainausmerkkien, joko yksittäisten (' ') tai kaksois (" "), poistamista käsiteltävästä tekstidatasta. Ohjelmoijien on usein tarpeen tehdä tämä datan siivoamiseksi, valmistautuakseen edelleen käsittelyyn tai kun lainausmerkit eivät ole oleellisia datan merkitykselle.

## Miten:

Tässä on yksinkertainen tapa poistaa molemmat lainausmerkityypit merkkijonosta Kotlinissa:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // Tuloste: Kotlin rocks its cool
}
```

Ja jos haluat poistaa vain toisen tyyppiset lainausmerkit, jätä toinen replace-kutsu väliin.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // Tuloste: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // Tuloste: Kotlin "rocks" its cool
}
```

## Syväluotaus

Historiallisesti merkkijonojen käsittely ja merkkien poistaminen ovat olleet ohjelmoinnin ydinosa, koska teksti on perustava tapa, jolla olemme vuorovaikutuksessa datan kanssa. Joskus merkkijonoissa olevat lainausmerkit on pakko poistaa. Tämä osoitetaan edeltävällä kenoviivalla (esim., `"Hän sanoi, \"Hei!\""`). Tällaisten merkkijonojen käsittelemisessä saatat tarvita poistamaan paitsi paon merkit, myös lainausmerkit itsessään saadaksesi puhtaamman tai käyttökelpoisemman tekstin.

Vaihtoehtoja `replace`-metodille sisältävät regex-pohjaisen poiston tai manuaalisen merkkijonon jäsentämisen merkki kerrallaan. Kuitenkin, regex voi olla ylilyönti yksinkertaisissa operaatioissa, ja manuaalinen jäsentäminen vähemmän tehokasta kuin valmiiden merkkijonofunktioiden käyttö. Kotlinin `replace`-funktio hyödyntää alla olevaa Javan `String` `replace`-metodia, joka on hyvin optimoitu suorituskyvylle.

Toteutuksen kannalta on mainitsemisen arvoista, että Kotlin on yhteensopiva Javan kanssa, joten käytännössä kaikki merkkijonoihin kohdistuvat operaatiot ovat yhtä suorituskykyisiä kuin Javassa. Lainausmerkkien poistettaessa on tärkeää olla tietoinen reunatapauksista, kuten sisäkkäisistä lainausmerkeistä, jotka saattavat vaatia monimutkaisempaa lähestymistapaa, mahdollisesti käyttäen säännöllisiä lausekkeita tai jäsentämiskirjastoa.

## Katso Myös

Lisätietoa merkkijonojen käsittelystä Kotlinissa, voit tarkistaa viralliset dokumentaatiot:

- [Kotlinin String dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

Syvempiin sukelluksiin säännöllisistä lausekkeista ja jäsentämisestä Kotlinissa:

- [Kotlin Regex dokumentaatio](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
