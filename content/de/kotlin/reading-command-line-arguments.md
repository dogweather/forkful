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

# Was & Warum?

Das Einlesen von Befehlszeilenargumenten ist ein wichtiger Bestandteil der Programmierung in Kotlin. Dabei werden die vom Nutzer eingegebenen Argumente, die zusammen mit dem Programmnamen in der Kommandozeile ausgeführt werden, in Variablen gespeichert. Dies ermöglicht es, dass das Programm auf die Eingaben des Nutzers reagieren und entsprechende Aktionen ausführen kann.

Programmierer nutzen das Einlesen von Befehlszeilenargumenten, um die Flexibilität ihrer Programme zu erhöhen. Durch die Verwendung von Variablen können Nutzer die Ausführung des Programms individuell anpassen, ohne jedes Mal den Quellcode ändern zu müssen.

# Wie geht's?

Um Befehlszeilenargumente in Kotlin einzulesen, muss zuerst eine ```main()``` Funktion erstellt werden. Innerhalb dieser Funktion kann die Methode ```args``` verwendet werden, um auf die Argumente zuzugreifen. Einzelne Argumente können dann durch Angabe der entsprechenden Indexposition ausgelesen werden.

Beispielcode:

```Kotlin
fun main(args: Array<String>) {
   val argument1 = args[0]
   val argument2 = args[1]
   println("Argument 1: $argument1")
   println("Argument 2: $argument2")
}
```

Eingabe in der Kommandozeile:

```
kotlin-programm argument1 argument2
```
Ausgabe:

```
Argument 1: argument1
Argument 2: argument2
```

# Tiefgehende Einblicke

Das Einlesen von Befehlszeilenargumenten ist eine grundlegende Funktion in vielen Programmiersprachen. Es ermöglicht die Interaktion mit dem Nutzer und die Anpassung des Programms. Alternativ kann auch über die Java-Klasse ```Scanner``` auf die Eingaben des Nutzers zugegriffen werden.

In Kotlin gibt es auch die Möglichkeit, benannte Argumente zu verwenden, indem vor dem Argumentname ein Doppelpunkt gesetzt wird, z.B. ```:argument1```. Dies kann die Lesbarkeit des Codes verbessern und das Einlesen von Argumenten erleichtern.

Die Implementierung des Einlesens von Befehlszeilenargumenten in Kotlin ist einfach und unkompliziert. Es gibt jedoch einige Punkte zu beachten, zum Beispiel die Indexposition der Argumente. Auch sollte die Eingabe des Nutzers immer auf Korrektheit überprüft werden, um fehlerhafte Eingaben zu vermeiden.

# Weitere Quellen

- Offizielle Kotlin Dokumentation: https://kotlinlang.org/docs/command-line.html
- Tutorial zum Einlesen von Befehlszeilenargumenten in Kotlin: https://www.javatpoint.com/kotlin-command-line-arguments
- Vergleich zwischen der Verwendung von ```args``` und ```Scanner```: https://stackoverflow.com/questions/33469951/what-is-the-difference-between-args-and-a-scanner-for-reading-command-line-argu