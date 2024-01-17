---
title:                "Eine Textdatei schreiben."
html_title:           "Clojure: Eine Textdatei schreiben."
simple_title:         "Eine Textdatei schreiben."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

Was und Warum? 
Das Schreiben von Textdateien ist ein grundlegender Bestandteil des Programmierens und beinhaltet das Erstellen und Speichern von Textinhalten in einer Datei. Programmierer nutzen Textdateien, um Daten in einer leicht lesbaren und einfach zu manipulierenden Form zu speichern. 

Wie geht's:
(Benutzen Sie Clojure-Codeblöcke, um Beispiele und Ausgaben darzustellen)

```Clojure 
;; Eine Textdatei erstellen und schreiben 
(with-open [f (clojure.java.io/writer "meineDatei.txt")]
  (.write f "Dies ist ein Beispieltext, der in meineDatei.txt geschrieben wird."))

;; Eine bestehende Textdatei lesen 
(with-open [r (clojure.java.io/reader "meineDatei.txt")]
  (println (.readLine r)))
```

Ausgabe: 
Dies ist ein Beispieltext, der in meineDatei.txt geschrieben wird. 

Tief einsteigen:
Das Schreiben von Textdateien ist ein grundlegender Teil des Programmierens, der schon seit langer Zeit verwendet wird. Alternativ können auch Datenbanken oder andere Dateiformate verwendet werden, aber das Schreiben von Textdateien ist eine einfache und effiziente Möglichkeit, Daten zu speichern. Die Implementation des Schreibens einer Datei in Clojure basiert auf Standardbibliotheken für den Umgang mit Dateien und ist somit einfach zu erlernen und anzuwenden. 

Weitere Informationen:
https://clojure.org/reference/java_interop
https://clojure.org/guides/io

Siehe auch:
https://github.com/greglook/clj-new/example-app
https://stackoverflow.com/questions/11437169/creating-and-writing-to-a-file-in-clojure