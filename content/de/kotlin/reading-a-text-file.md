---
title:                "Das Lesen einer Textdatei"
html_title:           "Kotlin: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

Was ist das Lesen einer Textdatei?

Das Lesen einer Textdatei ist ein wichtiger Teil der Programmierung, bei dem der Inhalt einer Datei ausgelesen und verarbeitet wird. Programmierer nutzen dies, um Informationen aus externen Quellen wie Konfigurationsdateien oder Nutzereingaben zu erhalten.

Wozu machen Programmierer das?

Das Lesen von Textdateien ermöglicht es Programmierern, auf einfache und effiziente Weise wichtige Daten zu erhalten, die ihr Programm benötigt. Diese externe Speicherung ermöglicht es, dynamischere Anwendungen zu erstellen, die sich an die Bedürfnisse der Benutzer anpassen können.

So geht's: 

Ein Beispiel, wie man das Lesen einer Textdatei in Kotlin implementieren kann:

```
val file = File("meinetextdatei.txt")
val text = file.readText()
println(text)
```

Im obigen Beispiel wird zuerst eine Datei-Instanz mit dem Namen der zu lesenden Datei erstellt. Dann wird die Funktion "readText()" verwendet, um den gesamten Inhalt der Datei in eine String Variable zu speichern. Zum Schluss wird der Inhalt in der Konsole ausgegeben.

Eine andere Möglichkeit ist die Verwendung der "readLines()" Funktion, um den Inhalt in einer Liste von Strings zu speichern:

```
val file = File("meinetextdatei.txt")
val lines = file.readLines()
for (line in lines) {
    println(line)
}
```

Tiefer tauchen:

Das Lesen von Textdateien ist eine gängige Praxis, die seit den Anfängen der Programmierung verwendet wird. Früher wurde dies hauptsächlich verwendet, um Daten von externen Quellen wie Dateisystemen und Datenbanken zu erhalten. Heute gibt es jedoch auch moderne Alternativen wie APIs und Web-Services, die ähnliche Funktionen bieten.

Für die Implementierung gibt es mehrere Möglichkeiten, je nachdem was man mit dem Inhalt der Textdatei machen möchte. Das Lesen einer Textdatei kann mit integrierten Funktionen wie "readText()" und "readLines()" erfolgen, es gibt jedoch auch Bibliotheken und Frameworks wie Apache Commons IO oder Kotlin-io, die erweiterte Funktionen anbieten und das Lesen in verschiedenen Formaten wie CSV oder JSON ermöglichen.

Siehe auch:

- [Kotlin.io](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/)
- [Kotlin-CSV](https://github.com/Steveice10/Kotlin-CSV)