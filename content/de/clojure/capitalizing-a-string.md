---
title:                "Einen String großschreiben"
aliases:
- de/clojure/capitalizing-a-string.md
date:                  2024-02-03T19:04:50.247898-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren eines Strings beinhaltet die Modifikation des Strings, sodass sein erstes Zeichen großgeschrieben wird, während der Rest des Strings unverändert bleibt. Programmierer führen oft die Großschreibung von Strings durch, um die Datenkonsistenz zu gewährleisten, insbesondere bei Namen und Orten oder um grammatikalischen Regeln in Benutzeroberflächen zu entsprechen.

## Wie:
Clojure, als eine JVM-Sprache, ermöglicht es Ihnen, Java-String-Methoden direkt zu nutzen. Hier ist ein grundlegendes Beispiel, wie man einen String in Clojure kapitalisiert:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure enthält keine eingebaute Funktion speziell für das Kapitalisieren von Strings, aber wie gezeigt, können Sie dies leicht erreichen, indem Sie `clojure.string/upper-case`, `subs` und `str` Funktionen kombinieren.

Für eine prägnantere Lösung und die Handhabung komplexerer String-Manipulationen könnten Sie sich an eine Drittanbieterbibliothek wenden. Eine solch beliebte Bibliothek im Clojure-Ökosystem ist `clojure.string`. Allerdings bietet sie, soweit ich bis zum letzten Update informiert bin, keine direkte `capitalize` Funktion über die mit den Kernfunktionen von Clojure demonstrierten Möglichkeiten hinaus an, sodass die oben gezeigte Methode Ihr direkter Ansatz ist, ohne zusätzliche Bibliotheken speziell für die Kapitalisierung einzubeziehen.

Denken Sie daran, wenn Sie mit Strings in Clojure arbeiten, die mit Java-Methoden interagieren, arbeiten Sie effektiv mit Java-Strings, was es Ihnen ermöglicht, das gesamte Arsenal der Java-String-Methoden direkt in Ihrem Clojure-Code zu nutzen, falls notwendig.
