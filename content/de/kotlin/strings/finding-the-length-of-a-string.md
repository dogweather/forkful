---
date: 2024-01-20 17:47:35.832577-07:00
description: "How to: Hier ein schnelles Beispiel, wie man die L\xE4nge eines Strings\
  \ in Kotlin findet."
lastmod: '2024-03-13T22:44:53.838266-06:00'
model: gpt-4-1106-preview
summary: "Hier ein schnelles Beispiel, wie man die L\xE4nge eines Strings in Kotlin\
  \ findet."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## How to:
Hier ein schnelles Beispiel, wie man die Länge eines Strings in Kotlin findet:

```kotlin
fun main() {
    val begruessung = "Hallo Welt!"
    println("Die Länge des Strings ist: ${begruessung.length}")
}

// Ausgabe:
// Die Länge des Strings ist: 11
```

## Deep Dive
In Kotlin läuft es unter der Haube so ab, dass die `.length`-Eigenschaft eines Strings dessen `length`-Feld aus der Java-Stringklasse abruft. Historisch stammt diese Eigenschaft aus Java, da Kotlin darauf aufbaut und problemlos mit Java-Code interoperiert.

Alternativen? Man kann auch manuell durch den String iterieren und zählen – aber warum das Rad neu erfinden? Kotlin bringt bereits alles Nötige mit.

Einen wichtigen Punkt gibt es allerdings: Unicode. Bei Zeichen, die als Surrogate Pairs repräsentiert werden, kann `.length` irreführend sein, da es die Anzahl der `Char`-Einheiten zurückgibt und nicht die tatsächliche Anzahl der Codepunkte. Für die meisten Anwendungsfälle ist dies aber ein Randthema.

## See Also
- Kotlin-Standardbibliothek zur String-Behandlung: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Unicode in Kotlin verstehen: [https://kotlinlang.org/docs/characters.html](https://kotlinlang.org/docs/characters.html)
