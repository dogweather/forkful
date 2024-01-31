---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?

Schreiben auf Standardfehler (stderr) leitet kritische Nachrichten aus deinem Programm, abgetrennt von der Hauptausgabe (stdout). Es hilft bei der Diagnose von Problemen, indem Fehlermeldungen getrennt bleiben und leichter zu erkennen sind.

## How to:

Clojure verwendet Java's System-Klassen, um auf `stderr` zu schreiben. Hier ist ein einfaches Beispiel:

```Clojure
(defn schreibe-auf-stderr [nachricht]
  (.println System/err nachricht))

(schreibe-auf-stderr "Fehler gefunden!")
```

Die Ausgabe ist nicht sichtbar im normalen Programmauslauf, aber sie erscheint auf stderr:

```
Fehler gefunden!
```

## Deep Dive

Clojure, als eine JVM-Sprache, übernimmt Java's Standard-Datenströme `System.out` und `System.err` für die Ausgabe. Historisch gesehen stammt diese Trennung aus der Unix-Welt. Alternativ könntest du Logging-Frameworks verwenden, um Fehler zu protokollieren. Die `System/err`-Implementation ist synchron, was bedeutet, dass die Nachricht sofort geschrieben wird, aber es kann bei hohem Durchsatz zu Leistungsengpässen führen.

## See Also

1. Clojure's offizielle Dokumentation: [Clojure.org](https://clojure.org/)
2. Java API für `PrintStream`: [PrintStream JavaDocs](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
