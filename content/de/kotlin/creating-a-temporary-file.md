---
title:    "Kotlin: Erstellen einer temporären Datei"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man in der Programmierung temporäre Dateien erstellt. Einige häufige Gründe können sein: Zwischenspeichern von Daten, Speichern von Protokolldateien, Verarbeiten von großen Mengen an Daten, oder einfach nur zum Testen von Code.

# Wie man eine temporäre Datei erstellt

Um eine temporäre Datei in Kotlin zu erstellen, kann die `createTempFile()` -Funktion verwendet werden. Diese Funktion erstellt automatisch eine temporäre Datei in einem angegebenen Verzeichnis mit einem einzigartigen Namen. Hier ist ein Beispiel, wie man diese Funktion verwenden kann:

```Kotlin
val tempFile = createTempFile("sample", ".txt")
println("Name der erzeugten Datei: ${tempFile.name}")
println("Pfad der erzeugten Datei: ${tempFile.absolutePath}")
```

Die Ausgabe dieses Codes in einer IDE könnte beispielsweise so aussehen:

```txt
Name der erzeugten Datei: sample5976583996218194573.txt
Pfad der erzeugten Datei: /Users/name/Desktop/sample5976583996218194573.txt
```

Wie man sehen kann, wird die temporäre Datei mit einem eindeutigen Namen im angegebenen Verzeichnis erstellt. Diese Datei kann dann verwendet werden, um Daten zu speichern oder zu verarbeiten.

# Tiefere Einblicke

Die `createTempFile()` -Funktion verwendet einen Algorithmus, um sicherzustellen, dass der Name der temporären Datei eindeutig ist, auch wenn mehrere Threads gleichzeitig aufgerufen werden. Diese Funktion erstellt standardmäßig eine temporäre Datei im Standard-Temp-Verzeichnis des Betriebssystems. Sie können jedoch auch einen bestimmten Ordner angeben, in dem die Datei erstellt werden soll.

Eine weitere nützliche Funktion ist `createTempDirectory()`, die eine temporäre Verzeichnis zurückgibt, anstatt eine Datei zu erstellen. Diese Funktion kann verwendet werden, wenn Sie temporäre Dateien in einem Ordner organisieren möchten, anstatt sie direkt im Temp-Verzeichnis zu haben.

# Siehe auch

- [Kotlin Dokumentation über das Erstellen von temporären Dateien](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Creating Temporary Files in Java](https://www.baeldung.com/java-temporary-files)
- [Kotlin schnelle Einführung - Erste Schritte mit Dateisystemoperationen](https://kotlinlang.org/docs/tutorials/kotlin-for-py/files.html)