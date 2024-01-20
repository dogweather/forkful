---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Kommandozeilenargumenten ist das Abfangen von Eingaben, die bei der Ausführung eines Programms in der Kommandozeile gegeben werden. Programmer nutzen diese Technik, um die Interaktion mit dem Nutzer zu ermöglichen und die Ausführung des Programms zu steuern.

## Anleitung:
In Kotlin ist es ganz einfach, Kommandozeilenargumente zu lesen. Hier ist ein Schnellstart-Code:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

Führen Sie dieses Programm aus und geben Sie Argumente im Kommandozeilenfeld ein. Z.B.:

```Shell
kotlin MainKt Hallo Welt!
```

Dies gibt aus: 

```Shell
Hallo
Welt!
```

## Vertiefung
Historisch gesehen sind Kommandozeilenargumente ein altes Konzept, das von den ersten Unix-Shell-Interpreten stammt. In Kotlin ist das Standardarray `args` für die Eingabeaufforderungsargumente reserviert.

Es gibt auch Alternativen zum manuellen Parsen von Kommandozeilenargumenten, wie z.B. die Verwendung von Bibliotheken wie Apache Commons CLI oder JCommander.

Die interne Implementierung zum Lesen von Befehlszeilenargumenten in Kotlin geht von Java aus, da Kotlin auf der Java Virtual Machine (JVM) ausgeführt wird. Daher ähnelt der Zugriff auf die Befehlszeilenargumente dem in Java, nur mit mehreren sprachspezifischen Verknüpfungen zur Verbesserung des Benutzererlebnisses.

## Siehe auch
Schauen Sie sich diese Quellen an für weitere Informationen:
- Kotlin Dokumentation: https://kotlinlang.org/docs/tutorials/command-line.html
- Apache Commons CLI: https://commons.apache.org/proper/commons-cli/
- JCommander: http://jcommander.org/