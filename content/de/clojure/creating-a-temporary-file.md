---
title:    "Clojure: Erstellen einer temporären Datei"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen temporärer Dateien ist eine häufige Aufgabe in der Softwareentwicklung. Oft muss man Daten vorübergehend speichern oder verarbeiten, bevor sie an ihren endgültigen Speicherort übertragen werden können. In diesem Blogbeitrag werden wir uns ansehen, wie man in Clojure temporäre Dateien erstellen kann.

## Wie man temporäre Dateien in Clojure erstellt

Das Erstellen temporärer Dateien in Clojure ist relativ einfach. Wir werden das `clojure.java.io`-Paket verwenden, um die Datei zu erstellen. Hier ist ein Beispielcode, der eine temporäre Datei erstellt:

```Clojure
(require '[clojure.java.io :as io])

(with-open [temp-file (io/file "temp" (str (rand-int 10000) ".txt"))]
  (println "Temporäre Datei erstellt: " (.getPath temp-file)))
```

In diesem Beispiel verwenden wir zunächst die `require`-Funktion, um das `clojure.java.io`-Paket zu importieren, das wir verwenden werden. Dann verwenden wir die `with-open`-Makro, um sicherzustellen, dass die Datei automatisch geschlossen wird, sobald der Code innerhalb der Klammern beendet ist. Innerhalb des `with-open`-Blocks erstellen wir eine temporäre Datei mit dem Namen "temp" und einer zufälligen Nummer als Dateinamen. Als nächstes verwenden wir `println`, um den Pfad der erstellten temporären Datei auszugeben.

Wenn wir diesen Code ausführen, erhalten wir folgende Ausgabe:

```
Temporäre Datei erstellt: /var/folders/8q/mb8yks4x3qzfbr0f5lqmqbj00000gn/T/temp4260.txt
```

Wie Sie sehen können, haben wir erfolgreich eine temporäre Datei mit einem zufälligen Namen in unserem temporären Ordner erstellt.

## Tiefer gehende Informationen

Das `with-open`-Makro ist sehr nützlich, da es sicherstellt, dass die erstellte Datei automatisch geschlossen wird, um Speicherlecks zu vermeiden. Wenn Sie jedoch mehr Kontrolle über die erstellte Datei benötigen, gibt es noch andere Funktionen, die Sie verwenden können.

Eine Möglichkeit ist die Verwendung der `temp-file`-Funktion, die nur eine temporäre Datei erstellt, aber nicht automatisch geschlossen wird. Dies kann nützlich sein, wenn Sie die Datei in einem späteren Zeitpunkt in Ihrer Codebasis verwenden müssen.

Eine weitere Möglichkeit besteht darin, die `delete-on-exit`-Funktion zu verwenden, die dafür sorgt, dass die temporäre Datei automatisch gelöscht wird, wenn das Programm beendet wird. Dies hilft, den temporären Ordner sauber zu halten und Speicherplatz zu sparen.

Insgesamt ist das Erstellen temporärer Dateien in Clojure eine einfache, aber wichtige Aufgabe, die in vielen Anwendungen benötigt wird.

## Siehe auch

- [Offizielle Dokumentation zu `clojure.java.io`](https://clojuredocs.org/clojure.java.io/file)
- [Weitere Informationen über das Erstellen temporärer Dateien in Clojure](https://www.braveclojure.com/core-functions-file/)