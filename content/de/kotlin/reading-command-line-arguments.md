---
title:                "Kotlin: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Verarbeiten von Argumenten über die Befehlszeile ist ein wichtiger Teil der Programmierung in jeder Sprache, einschließlich Kotlin. Es ermöglicht es dem Benutzer, das Verhalten des Programms anzupassen und macht es flexibler.

## Wie Geht's

Um Argumente aus der Befehlszeile in Kotlin zu lesen, verwenden wir die `args`-Variable im `main`-Funktionsblock. Die Argumente werden als Array von Strings in der richtigen Reihenfolge übergeben.

```Kotlin
fun main(args: Array<String>) {
  // Code zum Lesen der Argumente
  for (arg in args) {
    println(arg)
  }
}
```

Der Codeblock liest alle Argumente und gibt sie einzeln aus. Wenn wir also `kotlin main.kt argument1 argument2` in der Befehlszeile ausführen, wird `argument1` und `argument2` ausgegeben.

## Tief eintauchen

Wir können auch prüfen, ob die Argumente bestimmte Bedingungen erfüllen oder spezielle Aktionen ausführen sollen. Beispielsweise können wir mithilfe von `when`-Statements entscheiden, was mit den Argumenten geschehen soll.

```Kotlin
fun main(args: Array<String>) {
  when (args[0]) {
    "uhrzeit" -> println("Es ist ${Date()}")
    "name" -> println("Hallo, ${args[1]}!")
    "quadrat" -> println("Das Quadrat von ${args[1]} ist ${args[1].toInt()*args[1].toInt()}")
    else -> println("Das Argument ${args[0]} ist ungültig.")
  }
}
```

Wenn wir nun beispielsweise `kotlin main.kt uhrzeit` ausführen, wird die aktuelle Uhrzeit ausgegeben. Oder wenn wir `kotlin main.kt name Max` ausführen, wird uns mit "Hallo, Max!" geantwortet.

## Siehe Auch

- Kotlin-Referenz für `args`: https://kotlinlang.org/docs/tutorials/command-line.html#reading-command-line-arguments