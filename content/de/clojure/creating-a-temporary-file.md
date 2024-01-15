---
title:                "Erstellen einer temporären Datei"
html_title:           "Clojure: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Die Erstellung temporärer Dateien ist ein wichtiger Teil der Entwicklung in jeder Programmiersprache, einschließlich Clojure. Diese Art von Dateien werden verwendet, um vorübergehend Daten zu speichern, die während der Programmausführung benötigt werden. Sie sind besonders nützlich, um temporäre Daten in einer sicheren und effizienten Weise zu handhaben, ohne dass sie dauerhaft im System gespeichert werden müssen.

## Wie das geht

Das Erstellen einer temporären Datei in Clojure ist einfach und unkompliziert. Wir können die "tempfile" Funktion aus der "java.io" Bibliothek verwenden, um eine temporäre Datei mit einem eindeutigen Namen zu erzeugen. Die Syntax sieht folgendermaßen aus:

```Clojure
(require '[clojure.java.io :as io])
(io/file (io/tmpdir) "meine_temporäre_datei.txt")
```
Dies wird eine temporäre Datei mit Namen "meine_temporäre_datei.txt" im temporären Verzeichnis des Systems erstellen.

Wir können auch angeben, in welchem Verzeichnis die temporäre Datei erstellt werden soll, indem wir den Pfad als zweites Argument an die "file" Funktion übergeben:

```Clojure
(io/file "C:/Temp" "meine_temporäre_datei.txt")
```

Um auf die temporäre Datei zuzugreifen, können wir die "slurp" Funktion verwenden, die den gesamten Inhalt der Datei als String zurückgibt. Wir können auch die "delete-file" Funktion verwenden, um die temporäre Datei zu löschen, wenn sie nicht mehr benötigt wird.

Eine weitere nützliche Funktion ist "with-tempfile", mit der eine temporäre Datei automatisch gelöscht wird, sobald sie nicht mehr benötigt wird. Diese Funktion erwartet zwei Argumente: einen Dateinamen und eine Funktion, die auf die temporäre Datei zugreift. Hier ist ein Beispiel:

```Clojure
(with-tempfile ["meine_temporäre_datei.txt" f]
  (spit f "Dies ist ein Beispielinhalt")
  (slurp f))
```

Dies wird eine temporäre Datei erstellen, in der der Text "Dies ist ein Beispielinhalt" geschrieben wird und dann durch die "slurp" Funktion zurückgegeben wird. Außerdem wird die Datei automatisch gelöscht, nachdem die Funktion ausgeführt wurde.

## Tiefer eintauchen

Das Erstellen von temporären Dateien kann auch mit dem "manipulate" Namespace in der "clojure.contrib.io" Bibliothek erfolgen. Dies ermöglicht eine größere Kontrolle über die Arbeit mit Dateien, einschließlich der Möglichkeit, das Verhalten von temporären Dateien anzupassen.

Die "io" Bibliothek bietet auch Funktionen für das Erstellen und Lesen von temporären Verzeichnissen, die ähnlich wie die Funktionen für Dateien funktionieren.

Egal für welche Methode Sie sich entscheiden, das Erstellen von temporären Dateien ist eine wichtige Fähigkeit, die jeder Clojure-Entwickler beherrschen sollte.

## Siehe auch

- Offizielle Clojure-Dokumentation: https://clojure.org/
- Beitrag über Clojure und Dateizugriff: https://clojure.org/guides/io
- Clojure-Bibliothek "manipulate": https://clojure.github.io/manifold/