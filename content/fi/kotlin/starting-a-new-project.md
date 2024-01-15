---
title:                "Aloittamassa uutta projektia"
html_title:           "Kotlin: Aloittamassa uutta projektia"
simple_title:         "Aloittamassa uutta projektia"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Miksi aloittaa uusi projekti? Lyhyesti sanottuna, uuden projektin aloittaminen antaa sinulle mahdollisuuden kehittää uusia taitoja, luoda jotain uutta ja haastaa itseäsi.

## Kuinka aloittaa

Kotlin on moderni ja helppokäyttöinen ohjelmointikieli, joka sopii erinomaisesti uusien projektien aloittamiseen. Tässä on muutama esimerkki, kuinka voit aloittaa oman Kotlin-projektisi:

```Kotlin
fun main() {
    // Tulostaa "Tervetuloa uuteen projektiin!"
    println("Tervetuloa uuteen projektiin!")
}
```

```Kotlin
// Määritellään funktio, joka palauttaa annetut parametrit yhteenlaskuna
fun sum(x: Int, y: Int): Int {
    return x + y
}

fun main() {
    // Kutsutaan sum-funktiota ja tulostetaan tulos
    val result = sum(2, 3)
    println("2 + 3 = $result")
}
```

```Kotlin
// Luodaan luokka, jossa on ominaisuus ja metodi
class Dog(val name: String) {

    // Metodi, joka haukkuu annetun ääniä annetun kerran
    fun bark(times: Int, sound: String) {
        for (i in 1..times) {
            println("$sound!")
        }
    }
}

fun main() {
    // Luodaan uusi koira-instanssi ja kutsutaan metodia
    val myDog = Dog("Luna")
    myDog.bark(3, "hau")
}
```

### Syöte ja tulostus

Kotlinilla voit myös kysyä käyttäjältä syötettä ja tulostaa vastauksen konsoliin:

```Kotlin
fun main() {
    // Pyydetään käyttäjää antamaan nimi
    println("Anna nimesi:")
    val name = readLine()
    // Tulostetaan tervehdys annetulla nimellä
    println("Hei $name, tervetuloa uuteen projektiin!")
}
```

```Kotlin
fun main() {
    // Pyydetään käyttäjää antamaan kaksi lukua
    println("Anna kaksi lukua:")
    val num1 = readLine()?.toInt()
    val num2 = readLine()?.toInt()
    // Lasketaan ja tulostetaan summa
    val sum = num1?.plus(num2 ?: 0)
    println("Summa on $sum")
}
```

## Syvemmälle

Uuden projektin aloittaminen Kotlinilla voi aluksi tuntua hieman pelottavalta, mutta älä huoli, olemme täällä auttamassa! Tässä on joitain resursseja, jotka auttavat sinua oppimaan enemmän Kotlinista ja sen käytöstä uusien projektien aloittamiseen:

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/home.html)
- [Kotlin for Android Developers -kurssi Udacitylla](https://www.udacity.com/course/kotlin-for-android-developers--ud888)
- [Kotlin for Java Developers -kurssi Courseralla](https://www.coursera.org/learn/kotlin-for-java-developers)

## Katso myös

- [Kotlinin virallinen verkkosivu](https://kotlinlang.org/)
- [Kotlin Tutorials -sarja YouTubessa](https://www.youtube.com/playlist?list=PLXjb35uitdWHpKQnTY3zanp6-eGXHBCcC)