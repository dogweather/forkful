---
title:                "Das Verfassen einer Textdatei"
html_title:           "Clojure: Das Verfassen einer Textdatei"
simple_title:         "Das Verfassen einer Textdatei"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Eines der ersten Dinge, die man in der Programmierung lernt, ist das Schreiben von Textdateien. Es ist ein grundlegender Schritt beim Erstellen von Programmen, da es uns ermöglicht, Daten zu speichern und zu verarbeiten. Das Schreiben einer Textdatei ist also eine wichtige Fähigkeit, die jeder Programmierer beherrschen sollte.

## Wie es geht

Um eine Textdatei in Clojure zu schreiben, benötigen wir die `with-open` Funktion und die `spit` Funktion. Die `with-open` Funktion sorgt dafür, dass die Datei ordnungsgemäß geschlossen wird, wenn wir fertig sind. Die `spit` Funktion nimmt zwei Argumente entgegen: den Dateinamen und den zu schreibenden Inhalt.

```Clojure
(with-open [fw (spit "meine_datei.txt" "Hallo, Welt!")] 
  (println "Datei erfolgreich geschrieben!"))
```

Das obige Beispiel erstellt eine Datei mit dem Namen "meine_datei.txt" und dem Inhalt "Hallo, Welt!". Wenn wir den Inhalt einer bereits vorhandenen Datei überschreiben möchten, können wir die `spit` Funktion ohne die `with-open` Funktion verwenden.

```Clojure
(spit "meine_datei.txt" "Hallo, Welt!")
```

In diesem Beispiel überschreiben wir den Inhalt der Datei "meine_datei.txt" mit dem neuen Inhalt "Hallo, Welt!".

## Tiefer Tauchgang

Es gibt noch einige weitere Parameter, die wir der `spit` Funktion übergeben können, um bestimmte Einstellungen für die Datei festzulegen. Zum Beispiel können wir die Option `:append true` verwenden, um den neuen Inhalt an das Ende der Datei anzuhängen, anstatt ihn zu überschreiben. Wir können auch ein drittes Argument übergeben, um die Kodierung der Datei anzugeben, z.B. `:encoding "UTF-8"`.

```Clojure
(with-open [fw (spit "meine_datei.txt" "Hallo, Welt!" :append true :encoding "UTF-8")]
  (println "Neuer Inhalt an Datei angehängt!"))
```

Es ist auch möglich, mit der `pr` Funktion formatierte Daten in eine Datei zu schreiben. Diese Funktion gibt die formatierten Daten als Zeichenfolge zurück, die dann von der `spit` Funktion verwendet werden kann.

```Clojure
(spit "meine_datei.txt" (pr (range 1 11)))
```

Dieses Beispiel schreibt die Zahlen 1-10 in die Datei "meine_datei.txt", jede Zahl in einer neuen Zeile.

## Siehe auch

- [Clojure Dokumentation für `with-open`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/with-open)
- [Clojure Dokumentation für `spit`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/spit)
- [Clojure Dokumentation für `pr`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/pr)