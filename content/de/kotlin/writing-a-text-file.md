---
title:    "Kotlin: Textdatei schreiben"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben einer Textdatei kann nützlich sein, wenn man Daten speichern oder übertragen möchte, ohne eine Datenbank zu verwenden. Es ist auch ein grundlegendes Konzept in der Programmierung, das es ermöglicht, Text in Dateien zu verarbeiten.

# Wie geht man vor

Um eine Textdatei in Kotlin zu schreiben, gibt es mehrere Schritte, die befolgt werden müssen.

1. Als erstes müssen wir die `File()` Funktion verwenden, um eine neue Datei zu erstellen. Dies erfordert den Pfad zur Datei sowie den Dateinamen, den wir verwenden möchten. Zum Beispiel:

```Kotlin
val file = File("C:/MeineDateien/beispiel.txt")
```

2. Als nächstes öffnen wir die Datei zur Bearbeitung mit der Funktion `writer()`. Dies gibt uns einen `FileWriter`-Objekt zurück, das wir verwenden können, um Daten in die Datei zu schreiben.

```Kotlin
val writer = file.writer()
```

3. Jetzt können wir mithilfe der `write()` Funktion Daten in die Datei schreiben. Hier ist ein Beispiel für das Schreiben eines Strings:

```Kotlin
writer.write("Dies ist ein Beispieltext.")
```

4. Schließlich müssen wir die Datei schließen, um die Änderungen zu speichern. Dazu verwenden wir die `close()` Funktion auf unserem `FileWriter`-Objekt.

```Kotlin
writer.close()
```

# Tiefergehende Informationen

Es gibt noch weitere Optionen und Funktionen, die beim Schreiben von Textdateien in Kotlin verwendet werden können. Zum Beispiel können wir den `append` Parameter bei der `writer()` Funktion verwenden, um Daten an eine bereits existierende Datei anzuhängen, anstatt sie zu überschreiben.

```Kotlin
val writer = file.writer(append = true)
```

Wir können auch mit dem `charset` Parameter verschiedene Zeichencodierungen für unsere Daten wählen, und mit der `println()` Funktion anstelle von `write()` können wir automatisch einen Zeilenumbruch am Ende jeder Zeile hinzufügen.

# Siehe auch

- [`File()` Funktion](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/file.html)
- [`writer()` Funktion](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-writer/writer.html)
- [Kotlin - Die offizielle Website](https://kotlinlang.org/)