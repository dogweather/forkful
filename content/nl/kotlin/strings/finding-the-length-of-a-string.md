---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:11.512055-07:00
description: "Het vinden van de lengte van een string betekent het tellen van de tekens.\
  \ Programmeurs doen dit om invoer te valideren, door tekens te loopen of opslag\u2026"
lastmod: 2024-02-19 22:05:09.812297
model: gpt-4-0125-preview
summary: "Het vinden van de lengte van een string betekent het tellen van de tekens.\
  \ Programmeurs doen dit om invoer te valideren, door tekens te loopen of opslag\u2026"
title: De lengte van een string vinden
---

{{< edit_this_page >}}

## Wat & Waarom?
Het vinden van de lengte van een string betekent het tellen van de tekens. Programmeurs doen dit om invoer te valideren, door tekens te loopen of opslag toe te wijzen.

## Hoe:
```kotlin
fun main() {
    val groet = "Hallo, wereld!"
    println(groet.length)  // print 13
}
```
Uitvoer:
```
13
```

## Diepgaande Verkenning
In de begindagen van de informatica werden strings anders behandeld, vaak met null-afgesloten arrays in talen zoals C. Kotlin, als een moderne taal, biedt een ingebouwde `length` eigenschap voor String-objecten.

Alternatieven? Nou, je zou door een string kunnen loopen en tekens kunnen tellen - maar waarom het wiel opnieuw uitvinden? Kotlin's `length` is efficiënt en eenvoudig.

Onder de motorkap retourneert `length` de telling van UTF-16 code-eenheden in de string. Dit betekent dat voor de meeste tekst (zoals Engels), het aantal code-eenheden overeenkomt met het aantal tekens. Echter, voor tekens buiten het Basic Multilingual Plane (BMP), die worden vertegenwoordigd door twee code-eenheden (een surrogaat paar), komt de `length` eigenschap mogelijk niet overeen met het aantal Unicode codepunten.

## Zie Ook
- Kotlin Standaardbibliotheekreferentie voor Strings: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Begrip van UTF-16 en tekenrepresentatie: [Unicode in Java](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
- Een diepgaande verkenning naar Kotlin's behandeling van strings en gerelateerde functies: [Kotlin voor Java Ontwikkelaars](https://www.coursera.org/learn/kotlin-for-java-developers)
