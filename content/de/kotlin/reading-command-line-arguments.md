---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Kotlin: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn du mit Kotlin programmierst, wirst du früher oder später auf die Notwendigkeit stoßen, Kommandozeilenargumente zu lesen. Das kann zum Beispiel bei der Entwicklung von Konsolenanwendungen oder bei der Verwendung von externen Libraries notwendig sein. In diesem Artikel lernst du, wie du Kommandozeilenargumente in Kotlin lesen kannst.

## Anleitung

Um Kommandozeilenargumente in Kotlin zu lesen, muss zunächst die `main` Funktion deklariert werden. Diese wird automatisch von Kotlin ausgeführt, wenn das Programm gestartet wird. Zusätzlich müssen die Kommandozeilenargumente als Parameter der `main` Funktion angegeben werden.

```Kotlin
fun main(args: Array<String>) {
    // Code zum Lesen der Kommandozeilenargumente
}
```

Die `args` Variable ist ein Array von Strings, in dem alle eingegebenen Kommandozeilenargumente enthalten sind. Um auf ein bestimmtes Argument zuzugreifen, kann auf die entsprechende Position im Array zugegriffen werden. Zum Beispiel:

```Kotlin
fun main(args: Array<String>) {
    val ersteArgument = args[0]
    println("Das erste Argument ist $ersteArgument")
}
```

Wenn keine Kommandozeilenargumente angegeben werden, ist das `args` Array leer.

Um alle Argumente nacheinander auszugeben, kann eine `for` Schleife verwendet werden:

```Kotlin
fun main(args: Array<String>) {
    for (argument in args) {
        println(argument)
    }
}
```

Die Reihenfolge der Argumente entspricht der Reihenfolge, in der sie bei Aufruf des Programms eingegeben wurden.

## Tiefere Einblicke

In der `main` Funktion kann zusätzlich der `println` Befehl verwendet werden, um die Werte der einzelnen Argumente zu überprüfen. Außerdem können Bedingungen verwendet werden, um auf bestimmte Argumente zu reagieren oder diese weiterzuverarbeiten.

Es ist außerdem möglich, Optionen mit Werten zu definieren. In diesem Fall müssen die Argumente beim Aufruf des Programms in der Form "Option=Wert" angegeben werden. Im Code können dann entsprechende Bedingungen verwendet werden, um auf die Optionen und Werte zuzugreifen.

```Kotlin
fun main(args: Array<String>) {
    if (args.contains("-n")) {
        val name = args[args.indexOf("-n") + 1]
        println("Hallo $name!")
    } else {
        println("Willkommen!")
    }
}
```

Mit diesen Grundlagen kannst du nun Kommandozeilenargumente in deinem Kotlin Programm lesen.

## Siehe auch

- Offizielle Dokumentation zu Kotlin: https://kotlinlang.org/docs/home.html
- Tutorial zu Kotlin von JetBrains: https://www.jetbrains.com/help/idea/creating-and-running-your-first-kotlin-application.html
- Kotlin in Action Buch: https://www.manning.com/books/kotlin-in-action (in Englisch)