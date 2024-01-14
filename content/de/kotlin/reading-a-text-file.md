---
title:    "Kotlin: Das Lesen einer Textdatei"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen einer Textdatei ist eine grundlegende Fähigkeit für jeden Programmierenden und kann in verschiedenen Szenarien nützlich sein. Zum Beispiel können Sie eine Datei als Teil der Eingabe für Ihr Programm verwenden, um Daten auszulesen und zu verarbeiten. Oder Sie können eine aufbereitete Datei als Ausgabe schreiben, um von Ihrem Programm generierte Daten zu speichern. Egal aus welchem Grund, das Lesen einer Textdatei ist eine wichtige Fähigkeit, die es wert ist, sie zu erlernen.

# Wie geht man vor?

Um eine Textdatei in Kotlin zu lesen, gibt es verschiedene Schritte, die man befolgen muss.

1. Als erstes müssen wir eine Dateiinstanz mit dem Pfad zur Textdatei erstellen.
2. Dann müssen wir einen BufferedReader erstellen, der die Datei liest und in einen String umwandelt.
3. Nun können wir den BufferedReader in einer Schleife verwenden, um jeden Zeile der Datei zu lesen und die Informationen nach unseren Bedürfnissen zu verarbeiten.

Lass uns das Ganze in Codeblöcken sehen:

```Kotlin
// 1. Dateiinstanz erstellen
val file = File("pfad/zur/textdatei.txt")

// 2. BufferedReader erstellen
val reader = BufferedReader(FileReader(file))

// 3. Schleife durch jede Zeile der Datei
reader.forEachLine { line ->
    // Hier können wir die Zeile nach unseren Bedürfnissen verarbeiten
    println(line) // Zum Beispiel ausgeben
}
```

Die Ausgabe dieses Codes wird jede Zeile der Textdatei auf der Konsole ausgeben.

# Tiefentauchen

Es gibt noch einige weitere Aspekte, die beim Lesen einer Textdatei berücksichtigt werden sollten. Erstens müssen wir entscheiden, wie wir die Datei öffnen wollen. Die beiden gängigsten Methoden sind "readText()" und "forEachLine()", wobei letztere effizienter ist, weil sie die Datei Zeile für Zeile liest und somit die gesamte Datei nicht in den Speicher laden muss.

Zweitens müssen wir uns überlegen, wie wir mit Ausnahmen umgehen wollen, falls die Datei nicht gefunden werden konnte oder es Probleme beim Lesen gibt. Hier können wir "try-catch" Blöcke verwenden, um sicherzustellen, dass unser Programm nicht abstürzt und wir auf Fehlermeldungen reagieren können.

Und schließlich können wir auch Regex (reguläre Ausdrücke) verwenden, um bestimmte Informationen aus dem Text zu suchen und zu filtern.

Es gibt noch viel mehr zu entdecken, wenn es um das Lesen von Textdateien geht, aber mit diesen Grundlagen bist du bestens gerüstet, um damit zu beginnen.

# Siehe auch

- [Kotlin Dokumentation zum Lesen von Dateien](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html#read-text)
- [Java Tutorials zum Lesen von Textdateien](https://www.codecademy.com/articles/file-io-java) 
- [RegEx Cheatsheet](https://regex101.com/)

Happy coding!