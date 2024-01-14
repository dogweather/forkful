---
title:    "Kotlin: Ausgabe von Fehlersuche ausdrucken"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Sausgaben sind eine hilfreiche Methode, um den Programmierfluss zu verstehen, Fehler zu finden und die Funktionalität von Code zu überprüfen. Sie ermöglichen es, Variablenwerte und Codefortschritt zu verfolgen, um sicherzustellen, dass das Programm wie erwartet funktioniert.

## Wie geht's?

Um Debug-Sausgaben in Ihrem Kotlin-Code zu verwenden, müssen Sie zuerst die Standardbibliothek `kotlin.io` importieren. Dann können Sie die `println()`-Funktion verwenden, um eine Ausgabe auf der Konsole zu erstellen.

```Kotlin
import kotlin.io

fun main() {
    val name = "Sabine"
    println("Hallo, $name!")
}

// Ausgabe: Hallo, Sabine!
```

Sie können auch mehrere Variablen in einer Debug-Sausgabe kombinieren:

```Kotlin
val num1 = 5
val num2 = 7
println("Die Summe von $num1 und $num2 ist ${num1 + num2}.")

// Ausgabe: Die Summe von 5 und 7 ist 12.
```

Wenn Sie Ihre Ausgabe übersichtlicher gestalten möchten, können Sie auch die `println()`-Funktion mit der `if`-Anweisung verwenden:

```Kotlin
val num = 8
println("Die Zahl ist ${if (num % 2 == 0) "gerade" else "ungerade"}.")

// Ausgabe: Die Zahl ist gerade.
```

## Tiefer eintauchen

Es gibt viele Möglichkeiten, Debug-Sausgaben in Ihrem Kotlin-Code zu verwenden. Sie können auch die `readLine()`-Funktion verwenden, um Variableneingaben vom Benutzer zu erhalten und sie dann in einer Debug-Sausgabe zu verwenden.

```Kotlin
println("Bitte geben Sie Ihren Namen ein:")
val name = readLine()
println("Hallo, $name!")

// Eingabe "Sabine"
// Ausgabe: Bitte geben Sie Ihren Namen ein:
// Sabine
// Hallo, Sabine!
```

Debug-Sausgaben sind auch nützlich, wenn Sie komplexe Funktionen oder Schleifen in Ihrem Code haben. Sie können Ihre Ausgaben in verschiedenen Bereichen platzieren, um zu überprüfen, ob der Code wie erwartet funktioniert oder um Fehler zu finden:

```Kotlin
fun calculateAverage(numbers: List<Int>) {
    // Debug-Sausgabe vor der Schleife
    println("Die Liste enthält folgende Zahlen: $numbers.")
    var sum = 0
    for (num in numbers) {
        // Debug-Sausgabe innerhalb der Schleife
        println("Die Summe beträgt derzeit $sum.")
        sum += num
    }
    // Debug-Sausgabe nach der Schleife
    println("Die Summe der Zahlen ist $sum.")
    val average = sum / numbers.size
    println("Der Durchschnitt der Zahlen ist $average.")
}

calculateAverage(listOf(5, 9, 12, 4, 8))

// Ausgabe: Die Liste enthält folgende Zahlen: [5, 9, 12, 4, 8].
// Die Summe beträgt derzeit 0.
// Die Summe beträgt derzeit 5.
// Die Summe beträgt derzeit 14.
// Die Summe beträgt derzeit 26.
// Die Summe beträgt derzeit 30.
// Die Summe der Zahlen ist 30.
// Der Durchschnitt der Zahlen ist 6.
```

## Siehe auch

- [Offizielle Kotlin-Dokumentation zur `kotlin.io`-Bibliothek](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- [Tutorial zur Verwendung von Debug-Sausgaben in Kotlin](https://www.baeldung.com/kotlin/print-calls)
- [Weitere Ressourcen zum Debugging in Kotlin](https://www.raywenderlich.com/1161814-kotlin-debugging-tutorial)