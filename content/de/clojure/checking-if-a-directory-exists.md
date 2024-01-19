---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Clojure: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmierung prüfen wir oft, ob ein Verzeichnis existiert. Das ist wichtig, um Fehler zu vermeiden, wenn wir versuchen, Dateien zu lesen oder zu schreiben, die nicht da sind. 

## Wie funktioniert das:

Clojure ist elegant und prägnant. Hier ist ein einfacher Weg, um zu überprüfen, ob ein Verzeichnis in Clojure existiert. 

```Clojure
(ns directory-check.core
  (:require [clojure.java.io :as io]))

(defn directory-exists? [dir-path]
  (let [dir (io/file dir-path)]
    (and (.exists dir) (.isDirectory dir))))

(println (directory-exists? "/home/user/documents"))
```

Durch den Aufruf der Funktion mit dem Pfad zu dem Verzeichnis, das wir überprüfen möchten, wird entweder `true` (wenn es existiert) oder `false` (wenn es nicht existiert) ausgegeben.

## In die Tiefe:

Die Überprüfung der Existenz eines Verzeichnisses ist grundlegend für viele Anwendungen, daher war diese Funktion bereits in früheren Programmiersprachen vorhanden. Clojure, das auf Java basiert, ermöglicht dies auf ruhige und einfache Weise über das clojure.java.io-Modul.

Als Alternativen könnten Sie native Java-Aufrufe verwenden, aber Clojure's abstrakter Ansatz ist einfacher und idiomatischer. Es ist möglich, weitere Überprüfungen nach Bedarf hinzuzufügen (z.B. ob Sie Lese- oder Schreibzugriff haben).

## Weiterführende Links:

Clojure ist eine mächtige und ausdrucksstarke Sprache und hier sind einige Ressourcen, die Ihnen helfen könnten, tiefer in sie einzutauchen:

- [Offizielle Clojure-Dokumentation](https://clojure.org)
- [Clojure Über Clojure.java.io](https://clojuredocs.org/clojure.java.io)
- [Online-Kurs: Clojure for the Brave and True](https://www.braveclojure.com)