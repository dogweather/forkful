---
title:                "Eine Textdatei schreiben"
html_title:           "Kotlin: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Schreiben einer Textdatei handelt es sich um das Erstellen einer Datei, die menschenlesbaren Text enthält. Programmierer verwenden Textdateien, um Daten zu speichern oder um Informationen aus ihrem Programm auszugeben.

## Wie geht's?
Um eine Textdatei in Kotlin zu schreiben, gibt es verschiedene Ansätze. Eine Möglichkeit besteht darin, die `FileWriter`-Klasse zu verwenden, um eine neue Datei zu erstellen und Text in diese Datei zu schreiben. Das folgende Beispiel zeigt, wie es gemacht wird:

```Kotlin
fun main() {
    val file = FileWriter("neueDatei.txt")
    file.write("Das ist ein Beispieltext")
    file.close()
}
```
Dieser Code erstellt eine neue Datei mit dem angegebenen Dateinamen und schreibt den angegebenen Text in die Datei. Der `close()`-Befehl ist wichtig, um die Datei zu speichern und zu schließen.

Man kann auch die `writeText()`-Methode der `File`-Klasse verwenden, um eine Datei zu schreiben. Das folgende Beispiel zeigt, wie das funktioniert:

```Kotlin
fun main() {
    val text = "Das ist ein Beispieltext"
    val file = File("neueDatei.txt")
    file.writeText(text)
}
```

## Tief einstechen
Das Schreiben von Textdateien ist in der Programmierung seit langem eine gängige Praxis, da es eine einfache Möglichkeit bietet, Daten zu speichern und Informationen anzuzeigen. Eine alternative Möglichkeit, Text in einer Datei zu speichern, besteht darin, eine CSV- oder JSON-Datei zu verwenden. Diese Formate sind strukturierter und können verwendet werden, um Daten in einer Tabellenform oder in Objekten zu speichern.

In der Implementierung kann es hilfreich sein, die Charset-Einstellung anzugeben, z.B. `UTF-8`, um sicherzustellen, dass die Datei mit dem korrekten Zeichensatz erstellt wird. Außerdem ist es wichtig, sicherzustellen, dass die Dateiordner vorhanden sind und die entsprechenden Berechtigungen zum Schreiben von Dateien vorhanden sind.

## Siehe auch
- [Kotlin Dokumentation zum Schreiben von Dateien](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/write-text.html)
- [Java FileWriter Dokumentation](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)