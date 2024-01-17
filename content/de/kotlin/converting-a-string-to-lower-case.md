---
title:                "String in Kleinbuchstaben umwandeln"
html_title:           "Kotlin: String in Kleinbuchstaben umwandeln"
simple_title:         "String in Kleinbuchstaben umwandeln"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

#### Was & Warum?
Konvertierung von Zeichenketten zu Kleinbuchstaben (lower case) ist ein Prozess, bei dem alle Großbuchstaben in einer Zeichenkette in ihre Kleinbuchstabenform umgewandelt werden. Programmierer nutzen dies, um eine einheitliche und leichter lesbare Form von Text zu erhalten.

#### Anleitung:
```Kotlin 
// Beispiel 1: Verwendung von lower case-Funktion
val string = "Hallo, WELT!"
println(string.lowercase())
// Ausgabe: hallo, welt!

// Beispiel 2: Verwendung von toLowerCase-Funktion
val string = "Programmieren ist toll"
println(string.toLowerCase())
// Ausgabe: programmieren ist toll
```

#### Tiefere Einblicke:
1. Historischer Hintergrund: In der frühen Zeit der Computerprogrammierung wurden nur Großbuchstaben verwendet, wodurch die Zeichenketten schwer lesbar waren. Mit der Einführung von Kleinbuchstaben und der Möglichkeit, sie in Großbuchstaben umzuwandeln, wurden Texte leichter zu lesen.
2. Alternativen: Neben der Verwendung der ```lowercase()```- oder ```toLowerCase()```-Funktion gibt es auch die Option, jeden Buchstaben einzeln in Kleinbuchstaben umzuwandeln. Dies kann jedoch zeitaufwändig sein und ist daher nicht immer die beste Wahl.
3. Implementierungsdetails: Die Funktionen ```lowercase()``` und ```toLowerCase()``` nutzen den ASCII-Code der Zeichen, um sie in ihre Kleinbuchstabenform umzuwandeln. Dies bedeutet, dass nicht-ASCII-Zeichen (wie z.B. Umlaute) nicht korrekt konvertiert werden können.

#### Siehe auch:
- [Dokumentation von Kotlin zu ```lowercase()```](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- [Offizielle Kotlin-Dokumentation](https://kotlinlang.org/docs/home.html)
- [Alternative Lösung: Konvertierung von Kleinbuchstaben in Großbuchstaben](https://stackoverflow.com/questions/41107/how-do-i-convert-a-string-to-lower-case-in-android)