---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:36.179992-07:00
description: "Das Gro\xDFschreiben eines Strings in der Programmierung beinhaltet\
  \ das Umwandeln des ersten Zeichens des Strings in Gro\xDFbuchstaben, sofern dies\
  \ noch nicht\u2026"
lastmod: 2024-02-19 22:05:12.755638
model: gpt-4-0125-preview
summary: "Das Gro\xDFschreiben eines Strings in der Programmierung beinhaltet das\
  \ Umwandeln des ersten Zeichens des Strings in Gro\xDFbuchstaben, sofern dies noch\
  \ nicht\u2026"
title: "Einen String gro\xDFschreiben"
---

{{< edit_this_page >}}

## Was & Warum?

Das Großschreiben eines Strings in der Programmierung beinhaltet das Umwandeln des ersten Zeichens des Strings in Großbuchstaben, sofern dies noch nicht der Fall ist. Dies ist nützlich, um Benutzereingaben zu formatieren oder Text in einer Benutzeroberfläche auf eine standardisierte oder benutzerfreundliche Weise anzuzeigen. Programmierer führen diesen Vorgang durch, um die Datenkonsistenz zu gewährleisten oder um spezifische Formatierungsanforderungen innerhalb ihrer Softwareanwendungen zu erfüllen.

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
