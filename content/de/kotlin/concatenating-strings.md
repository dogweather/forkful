---
title:    "Kotlin: Zeichenketten verbinden"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen von Strings ist eine wichtige Funktion in vielen Programmiersprachen, einschließlich Kotlin. Es ermöglicht uns, unterschiedliche Zeichenketten miteinander zu verbinden, um längere und sinnvollere Nachrichten oder Ausgaben zu erstellen. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie wir Strings in Kotlin kombinieren können.

## How To

Um Strings in Kotlin zu kombinieren, können Sie die `+`-Operator verwenden oder die `plus()`-Funktion aufrufen. Schauen wir uns ein Beispiel an:

```kotlin
val name = "Anna"
val age = 25
val greeting = "Hallo, mein Name ist " + name + " und ich bin " + age + " Jahre alt."
```

In diesem Beispiel erstellen wir eine neue Variable `greeting`, die aus mehreren Strings zusammengesetzt ist, darunter der Name und das Alter einer Person. Beachten Sie, dass wir die Variablen `name` und `age` in die String-Zusammenstellung einfügen, indem wir sie mit `+` zusammenfügen.

Alternativ können wir auch die `plus()`-Funktion verwenden, um Strings zu kombinieren:

```kotlin
val name = "Anna"
val age = 25
val greeting = "Hallo, mein Name ist ".plus(name).plus(" und ich bin ").plus(age).plus(" Jahre alt.")
```

Beide Methoden liefern das gleiche Ergebnis. Der `+`-Operator ist jedoch einfacher und lesbarer zu schreiben.

## Deep Dive

Bei der Verwendung der `+`-Operator oder der `plus()`-Funktion zum Zusammenfügen von Strings müssen wir darauf achten, dass alle Werte vom Typ `String` oder `CharSequence` sind. Andernfalls können wir den Operator oder die Funktion nicht verwenden und einen Fehler erhalten. Zum Beispiel:

```kotlin
val name = "Anna"
val age = 25
val greeting = name + age // Fehler: Unresolved reference: plus
```

In diesem Beispiel ist `age` vom Typ `Int` und kann daher nicht direkt an einen String angehängt werden. Um dieses Problem zu lösen, müssen wir entweder `.toString()` auf das Alter anwenden oder eine String-Interpolation verwenden:

```kotlin
val name = "Anna"
val age = 25
val greeting = "Hallo, mein Name ist $name und ich bin $age Jahre alt."
```

Hier verwenden wir die `$`-Zeichen, um die Werte der Variablen `name` und `age` direkt in einen String einzubinden.

Zusätzlich zu `+` und `plus()` gibt es noch andere Kotlin-Funktionen zum Zusammenfügen von Strings, wie `concat()` und `joinToString()`. Diese Funktionen bieten zusätzliche Möglichkeiten und Parameter, um Strings auf verschiedene Weise zusammenzuführen. Wir empfehlen Ihnen, sich mit diesen Funktionen vertraut zu machen, um Ihre String-Kombinationsfähigkeiten zu erweitern.

## Siehe auch

- [Offizielle Dokumentation zu Strings in Kotlin](https://kotlinlang.org/docs/strings.html)
- [Tolle Kotlin-Tipps und Tricks](https://kotlinlang.org/docs/tutorials/kotlin-for-py/extra-features.html#awesome-style-with-concat-strings)
- [Interaktive Übungsbeispiele zum Kombinieren von Strings](https://play.kotlinlang.org/byExample/01_introduction/04_String_Templates)