---
date: 2024-01-26 03:45:48.568987-07:00
description: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n kokonaislukuun tai tiettyyn tarkkuuteen. Ohjelmoijat tekev\xE4\
  t niin parantaakseen\u2026"
lastmod: '2024-02-25T18:49:53.442825-07:00'
model: gpt-4-0125-preview
summary: "Numeroiden py\xF6rist\xE4minen tarkoittaa niiden s\xE4\xE4t\xE4mist\xE4\
  \ l\xE4himp\xE4\xE4n kokonaislukuun tai tiettyyn tarkkuuteen. Ohjelmoijat tekev\xE4\
  t niin parantaakseen\u2026"
title: "Numerojen py\xF6rist\xE4minen"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Numeroiden pyöristäminen tarkoittaa niiden säätämistä lähimpään kokonaislukuun tai tiettyyn tarkkuuteen. Ohjelmoijat tekevät niin parantaakseen luettavuutta, vähentääkseen tallennustilaa tai koska tarkka arvo ei ole kriittinen myöhemmissä laskelmissa.

## Miten:

Kotlinissa pyöristämisen voi tehdä käyttämällä useita funktioita, kuten `roundToInt()`, `roundToDouble()`, ja `BigDecimal` antaa enemmän hallintaa:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Tulostaa: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Tulostaa: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Tulostaa: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Tulostaa: 123.5
}
```

## Syväsukellus

Historiallisesti numeroiden pyöristäminen on ollut peruskäsite sekä matematiikassa että laskennassa, suunniteltu käsittelemään numeerisen tarkkuuden rajoituksia. Varhaisessa tietojenkäsittelyssä pyöristäminen oli kriittistä korkeiden muistin kustannusten vuoksi.

Kotlinissa pyöristäminen perustuu standardiin Java-kirjastoihin. Pyöristämisen vaihtoehtoja ovat `Math.round()`, joka pyöristää lähimpään kokonaislukuun, ja `BigDecimal` mukautettavaan pyöristämiseen, jossa voit määrittää mittakaavan ja `RoundingMode`.

Jokaisella `RoundingMode`-tilalla on erilaiset politiikat tasatilanteiden (kun numero on täsmälleen vaihtoehtojen keskellä pyöristettäessä) käsittelyyn. Esimerkiksi `RoundingMode.HALF_UP` pyöristää lähimpään naapuriin, paitsi jos molemmat naapurit ovat yhtä kaukana, jolloin se pyöristää ylöspäin.

## Katso Myös

- Kotlinin dokumentaatio [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oraclen Javan dokumentaatio [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- IEEE-standardi liukuluvuille (IEEE 754) [IEEE-standardi 754](https://ieeexplore.ieee.org/document/4610935)
