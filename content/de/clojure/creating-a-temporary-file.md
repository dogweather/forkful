---
title:    "Clojure: Erstellen einer temporären Datei"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Warum temporäre Dateien in Clojure erstellen?

Das Erstellen von temporären Dateien kann hilfreich sein, wenn du temporäre Daten speichern möchtest, die nur für eine bestimmte Zeit benötigt werden - z.B. beim Bearbeiten von Dateien oder beim Testen von Code. Temporäre Dateien werden automatisch gelöscht, wenn sie nicht mehr benötigt werden. In diesem Blogbeitrag werden wir uns ansehen, wie man in Clojure temporäre Dateien erstellen kann und was dabei zu beachten ist.

## Wie man temporäre Dateien in Clojure erstellt

Zuerst müssen wir das *java.io.File* Paket importieren, welches die Funktionalität bietet, temporäre Dateien zu erstellen. Dann können wir mit der Funktion *createTempFile* eine temporäre Datei erzeugen. Hier ist ein Beispielcode, der eine temporäre Datei erstellt und ihren Pfad ausgibt:

```Clojure
(import '[java.io File])

(def tmp-file (File/createTempFile "my-temp-" ".txt"))
(println (.getAbsolutePath tmp-file))
```
Die Ausgabe dieses Codes wird folgendermaßen aussehen:

```bash
/var/folders/6s/2ht4gfsj3sjf1/text1474639558782.txt
```

Wie du sehen kannst, wurde eine temporäre Datei mit einem Dateinamen, der mit "my-temp-" beginnt und ".txt" endet. Es ist wichtig bei dem Erstellen von temporären Dateien, dass du ein praefix und ein suffix an die *createTempFile* Funktion angibst, da diese beim Löschen der Datei verwendet werden. 

## Tiefere Einblicke in das Erstellen von temporären Dateien

Es ist wichtig zu beachten, dass die *createTempFile* Funktion standardmäßig die erstellte Datei im Standard-Temporärverzeichnis des Betriebssystems speichert. In Linux-Systemen ist dies normalerweise "/temp", in Windows "/Windows/Temp". Wenn du möchtest, dass die temporäre Datei in einem bestimmten Verzeichnis erstellt wird, kannst du das Verzeichnis als dritten Parameter an die Funktion übergeben.

Eine weitere wichtige Sache zu beachten ist, dass bei der Erstellung der temporären Datei, der Löschvorgang beim Beenden des Programms registriert wird. Dies bedeutet, dass selbst wenn das Programm unerwartet beendet wird, die erstellte temporäre Datei automatisch gelöscht wird.

Darüber hinaus gibt es auch eine Funktion *deleteOnExit*, mit der man eine manuelle Löschung beim Beenden des Programms verhindern kann. Sie kann insbesondere dann hilfreich sein, wenn du die temporäre Datei in einem bestimmten Verzeichnis erstellt hast und möchtest, dass sie auch nach dem Programmende erhalten bleibt.

# Siehe auch
* Clojure Java Interoperabilität - https://clojure.org/reference/java_interop
* Java File Dokumentation - https://docs.oracle.com/javase/7/docs/api/java/io/File.html
* Java 8 Tutorial - Erstellen von temporären Dateien - https://docs.oracle.com/javase/tutorial/essential/io/tmpfile.html