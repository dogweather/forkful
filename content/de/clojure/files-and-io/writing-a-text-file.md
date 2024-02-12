---
title:                "Eine Textdatei schreiben"
date:                  2024-02-03T19:27:32.362097-07:00
model:                 gpt-4-0125-preview
simple_title:         "Eine Textdatei schreiben"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei in Clojure umfasst das Erstellen oder Modifizieren von Dateien, um Daten außerhalb Ihrer Anwendung zu speichern, was Persistenz, Konfiguration, Protokollierung oder interprozessuale Kommunikation ermöglicht. Entwickler führen diese Aufgabe durch, um Zustände der Anwendung, Konfigurationen zu externalisieren oder Informationen zwischen verschiedenen Teilen eines Programms oder unterschiedlichen Programmen zu teilen.

## Wie geht das:

### Text mithilfe der integrierten Funktionen von Clojure in eine Datei schreiben

Die `spit`-Funktion ist der einfachste Weg, um Text in eine Datei in Clojure zu schreiben. Sie nimmt zwei Argumente entgegen: den Dateipfad und den zu schreibenden String. Wenn die Datei nicht existiert, wird `spit` sie erstellen. Tut sie das, wird `spit` sie überschreiben.

```clojure
(spit "example.txt" "Hallo, Welt!")
```

Um Text an eine bestehende Datei anzufügen, können Sie die `spit`-Funktion mit der Option `:append` verwenden.

```clojure
(spit "example.txt" "\nLass uns diese neue Zeile hinzufügen." :append true)
```

Nach dem Ausführen dieser Snippets wird "example.txt" enthalten:

```
Hallo, Welt!
Lass uns diese neue Zeile hinzufügen.
```

### Verwendung von Drittanbieterbibliotheken

Obwohl die integrierten Funktionen von Clojure oft ausreichen, hat die Community robuste Bibliotheken für komplexere oder spezifischere Aufgaben entwickelt. Für die Datei-E/A ist eine beliebte Bibliothek `clojure.java.io`, die einen eher Java-ähnlichen Ansatz für die Dateibehandlung bietet.

Um `clojure.java.io` für das Schreiben in eine Datei zu verwenden, müssen Sie es zunächst importieren:

```clojure
(require '[clojure.java.io :as io])
```

Danach können Sie die Funktion `writer` verwenden, um ein Writer-Objekt zu erhalten, und die Funktion `spit` (oder andere wie `print`, `println`), um in die Datei zu schreiben:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "Dies ist mit clojure.java.io geschrieben"))
```

Das wird (oder überschreibt, falls sie bereits existiert) "example_with_io.txt" mit dem Text erstellen:

```
Dies ist mit clojure.java.io geschrieben
```

Erinnern Sie sich: `with-open` stellt sicher, dass die Datei nach dem Schreiben ordnungsgemäß geschlossen wird und potenzielle Ressourcenlecks vermieden werden.
