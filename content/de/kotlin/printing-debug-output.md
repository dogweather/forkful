---
title:    "Kotlin: Debug-Ausgabe drucken"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Warum

Das Drucken von Debug-Ausgaben ist ein unverzichtbarer Bestandteil der Softwareentwicklung. Durch das Ausgeben von Variablenwerten oder Fehlermeldungen können Programmierer Probleme in ihrem Code schnell erkennen und beheben.

# Wie geht man vor

Um Debug-Ausgaben in Kotlin zu drucken, können Sie die `print()` oder `println()` Funktionen verwenden. Hier ist ein Beispiel:

```Kotlin
val name = "Max"
val age = 25
val height = 180
println("Name: $name, Alter: $age, Größe: $height")
```

Dieser Code würde folgende Ausgabe erzeugen:

```
Name: Max, Alter: 25, Größe: 180
```

Sie können auch mit der `format()` Funktion arbeiten, um die Ausgabe zu formatieren. Hier ist ein Beispiel, in dem wir die Höhe auf zwei Dezimalstellen runden:

```Kotlin
val heightInMeters = 1.8
println("Größe: %.2f m".format(heightInMeters))
```

Die Ausgabe wäre nun:

```
Größe: 1.80 m
```

# Tiefere Einblicke

Es gibt verschiedene Gründe dafür, warum man Debug-Ausgaben in der Entwicklung nutzen sollte. Zum einen kann es helfen, komplexe oder fehlerhafte Logik zu identifizieren. Zum anderen kann es auch bei der Fehlersuche in einer Live-Umgebung hilfreich sein, wo das Hinzufügen von Breakpoints möglicherweise nicht möglich ist.

Außerdem kann man mit Debug-Ausgaben auch interne Werte von Variablen überprüfen, um sicherzustellen, dass diese korrekt gesetzt sind. Man sollte jedoch beachten, dass zu viele Debug-Ausgaben die Performance der Anwendung beeinträchtigen können und daher sorgfältig eingesetzt werden sollten.

# Siehe auch

- [Offizielle Dokumentation zu Debugging in Kotlin](https://kotlinlang.org/docs/reference/java-to-kotlin-interop.html)
- [Tutorial zu Debugging mit IntelliJ IDEA](https://www.jetbrains.com/help/idea/debugging-your-first-java-application.html)
- [Blog-Artikel über Best Practices für Debugging-Ausgaben in Kotlin](https://blog.kotlin-academy.com/kotlin-debugging-for-beginners-cheat-sheet-dc95cdb69383)