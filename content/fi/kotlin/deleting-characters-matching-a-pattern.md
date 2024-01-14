---
title:                "Kotlin: Mallin mukaisten merkkien poistaminen"
simple_title:         "Mallin mukaisten merkkien poistaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Miksi #


Miksi Poistaa Merkkejä Tietyn Kaavan Mukaan

Monissa ohjelmointiprojekteissa saattaa olla tarvetta poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi liittyä esimerkiksi käyttäjien syöttämien tietojen validointiin tai tietojen jäsentelemiseen. Tässä blogikirjoituksessa esittelemme, miten tämä onnistuu Kotlin-kielen avulla.

## Kuinka Tehdä

Kotlinilla on useita eri tapoja poistaa merkkejä tietyn kaavan mukaan. Yksi tapa on käyttää `replace`-funktiota ja antaa sille kaava ja haluttu korvaava merkkijono parametreinä.

````Kotlin
val teksti = "Tämä on esimerkkiteksti!!"
val korjattuTeksti = teksti.replace(Regex("""[!.]"""), "")

println(korjattuTeksti)
````

Tämä koodi tuottaa seuraavan tulosteen:

```
Tämä on esimerkkiteksti
```

Kuten nähdään, kaikki `!` ja `.` merkit on poistettu korvaavasta merkkijonosta.

Toinen tapa on käyttää `filter`-funktiota ja antaa sille kaava lambda-lausekkeena. Tämä toimii hyvin myös merkkijonon sisältävien listojen kanssa.

````Kotlin
val sanalista = listOf("Moi", "Hei!", "Terve", "Tervehdys!!")
val siivottuLista = sanalista.filter { sana -> !sana.endsWith("!") }

println(siivottuLista)
````

Tämän koodin tulos on seuraava:

```
[Moi, Terve]
```

Tässä lambda-lausekkeessa `endsWith()`-funktiolla tarkistetaan, päättyykö sana `!`-merkkiin. Jos ei, sana sisällytetään uuteen siivottuun listaan.

## Syvällisempi Tarkastelu

Kotlinin `Regex`-luokka tarjoaa mahdollisuuden käyttää säännöllisiä lausekkeita merkkijonojen käsittelyssä. Näiden säännöllisten lausekkeiden avulla voidaan hakea ja korvata haluttuja merkkejä tai merkkijonoja.

Yllä esitellyssä esimerkissä käytetty säännöllinen lauseke `Regex("""[!.]""")` tarkoittaa, että etsitään kaikkia `!` ja `.` merkkejä merkkijonosta.

Tämä luokka tarjoaa monia muita hyödyllisiä ominaisuuksia, kuten mahdollisuuden etsiä tietystä kohdasta alkaen tai rajoittaa haettuja merkkejä tiettyyn määrään.

## Katso Myös

- [Kotlinin viralliset sivut](https://kotlinlang.org/)
- [Kotlinin dokumentaatio säännöllisistä lausekkeista](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)