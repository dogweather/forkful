---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:36.179992-07:00
description: "Wie: In Kotlin k\xF6nnen Strings mit den Standardbibliotheksfunktionen\
  \ ohne die Notwendigkeit von Drittanbieterbibliotheken gro\xDFgeschrieben werden.\
  \ Koltins\u2026"
lastmod: '2024-03-13T22:44:53.830730-06:00'
model: gpt-4-0125-preview
summary: "In Kotlin k\xF6nnen Strings mit den Standardbibliotheksfunktionen ohne die\
  \ Notwendigkeit von Drittanbieterbibliotheken gro\xDFgeschrieben werden."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie:
In Kotlin können Strings mit den Standardbibliotheksfunktionen ohne die Notwendigkeit von Drittanbieterbibliotheken großgeschrieben werden. Koltins Herangehensweise an die Behandlung von Strings macht diese Operationen unkompliziert und prägnant.

### Den gesamten String großschreiben:
```kotlin
val message = "hallo, welt!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Ausgabe: HALLO, WELT!
```

### Nur das erste Zeichen großschreiben:
Ab Kotlin 1.5 ist die Funktion `capitalize()` veraltet und wird durch eine Kombination aus `replaceFirstChar` und einem Lambda ersetzt, das überprüft, ob es sich um einen Kleinbuchstaben handelt, um ihn in Großbuchstaben umzuwandeln.

```kotlin
val greeting = "hallo, welt!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Ausgabe: Hallo, welt!
```

Dieser Ansatz behält den Rest des Satzes in seiner ursprünglichen Form bei und ändert nur den ersten Buchstaben in einen Großbuchstaben.
