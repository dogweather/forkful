---
title:    "Kotlin: Verketten von Zeichenfolgen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Strings zu concatenieren, also zu verketten, ist eine häufige Aufgabe in der Programmierung. Dies ist besonders nützlich, um Daten zu kombinieren oder benutzerdefinierte Ausgaben zu erstellen.

## Wie geht's

Zum Verketten von Strings verwenden wir den Operator "+" oder die Funktion "plus()". Betrachten wir das folgende Beispiel:

```Kotlin
val name = "Max"
val greeting = "Hallo "

val combinedString = greeting + name
println(combinedString) // Ausgabe: Hallo Max

val combinedString2 = greeting.plus(name)
println(combinedString2) // Ausgabe: Hallo Max
```

Hier kreieren wir zwei Variablen, jeweils mit einem String. Mit dem "+" Operator oder "plus()" Funktion können wir die beiden Strings miteinander verketten, indem wir sie einfach hintereinander schreiben. Wir können auch mehr als zwei Strings zusammenfügen, indem wir weiterhin "+" oder "plus()" verwenden.

Eine Alternative ist die Verwendung von String-Interpolation mit dem "$" Symbol. Betrachten wir das folgende Beispiel:

```Kotlin
val name = "Lisa"
val age = 25

val info = "Ich heiße $name und bin $age Jahre alt."
println(info) // Ausgabe: Ich heiße Lisa und bin 25 Jahre alt.
```

Hier verwenden wir die Variablen innerhalb des Strings und umschließen sie mit "$". Dadurch können wir sie direkt in den String einfügen, ohne "+" oder "plus()" verwenden zu müssen.

## Deep Dive

Beim Verketten von Strings gibt es einige wichtige Dinge zu beachten. Zum Beispiel sollten wir immer darauf achten, dass alle verwendeten Strings vom gleichen Typ sind. Eine Kombination von Strings und Zahlen kann unerwartete Ergebnisse liefern. Außerdem kann die Verwendung von "+" bei größeren Strings zu Performance-Problemen führen. In solchen Fällen ist es besser, die "StringBuilder" Klasse zu verwenden.

Eine weitere wichtige Sache ist, dass Strings in Kotlin unveränderlich (immutable) sind. Das bedeutet, dass jeder Versuch, einen String zu bearbeiten, tatsächlich einen neuen String erstellt und den ursprünglichen unverändert lässt. Es ist daher wichtig, dies bei der Verwendung von Verkettung zu berücksichtigen, um unnötigen Speicherverbrauch zu vermeiden.

## Siehe auch

- [Kotlin String Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin Basics: Strings](https://kotlinlang.org/docs/basic-syntax.html#strings)
- [Understanding String Pool in Java and its importance in memory management](https://www.baeldung.com/java-string-pool)