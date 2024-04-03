---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:06.291148-07:00
description: 'Hoe te: Zo laat je strings aan elkaar plakken in Kotlin - geen lijm
  nodig.'
lastmod: '2024-03-13T22:44:50.759510-06:00'
model: gpt-4-0125-preview
summary: Zo laat je strings aan elkaar plakken in Kotlin - geen lijm nodig.
title: Samenvoegen van strings
weight: 3
---

## Hoe te:
Zo laat je strings aan elkaar plakken in Kotlin - geen lijm nodig:

```kotlin
fun main() {
    val firstName = "Jet"
    val lastName = "Brains"
    val company = "Kotlin"

    // Met behulp van de plus operator
    val fullName = firstName + " " + lastName 
    println(fullName) // Uitvoer: Jet Brains

    // Met behulp van string templates
    val employeeIntro = "Hi, ik ben $firstName en ik werk bij $company."
    println(employeeIntro) // Uitvoer: Hi, ik ben Jet en ik werk bij Kotlin.

    // Met behulp van de concat() functie
    val product = "IntelliJ IDEA"
    val description = " is geweldig!"
    println(product.concat(description)) // Uitvoer: IntelliJ IDEA is geweldig!
}
```

## Diepere Duik
Samenvoeging bestaat al zolang we strings hebben om aan elkaar te knopen. Programmeertalen hebben constant de manier waarop ze deze taak behandelen geëvolueerd. In de begindagen, zou je muren van tekst vinden die samengevoegd werden met een simpele `+` operator. Spring vooruit naar modern Kotlin, en je hebt templates met `$` symbolen die variabelen magisch recht in de string trekken.

Er zijn alternatieven in overvloed. Als prestatie cruciaal is en je hebt te maken met een vrachtlading aan strings, kan StringBuilder je beste vriend zijn, door het vermijden van het creëren van meerdere stringobjecten. Dan is er de `joinToString` functie die een lijst neemt en deze samensmelt, gescheiden door een door jou gekozen scheidingsteken.

Elke methode heeft zijn eigen eigenaardigheden - `plus` is eenvoudig maar kan traag zijn bij overmatig gebruik; string templates zijn netjes voor leesbaarheid; `concat()` doet denken aan Java's methode en voelt een beetje formeel; `StringBuilder` en `joinToString` zijn performanter voor langdurige operaties.

## Zie Ook
Duik dieper in de wereld van Kotlin strings:

- [Kotlin Documentatie: Basistypen](https://kotlinlang.org/docs/basic-types.html#string-literals)
