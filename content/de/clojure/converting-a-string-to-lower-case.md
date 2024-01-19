---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmierung bedeutet das Umwandeln eines Strings in Kleinbuchstaben (lower case), dass alle alphabetischen Zeichen in einem String in ihre entsprechende Kleinbuchstabenform umgewandelt werden. Dies ist oft nützlich, um Nutzereingaben zu standardisieren oder um Zeichenkettenvergleiche ohne Berücksichtigung der Groß- und Kleinschreibung durchzuführen.

## So geht's:

Hier ist ein einfacher Codeausschnitt, der zeigt, wie man einen String in Kleinbuchstaben umwandelt, mit der Funktion `clojure.string/lower-case`:

```Clojure
(require '[clojure.string :as str])

(defn string-to-lower-case [s]
  (str/lower-case s))

(println (string-to-lower-case "CLOJURE IST COOL"))
```

Wenn du das obige Clojure-Programm ausführst, wirst du den folgenden Output sehen:

```Clojure
clojure ist cool
```

## Tiefere Einblicke:

Diese Operation zum Umwandeln von Zeichenketten in Kleinbuchstaben ist ein fundamentaler Bestandteil der meisten Programmiersprachen und wurde dementsprechend in fast jeder modernen Sprache eingebaut. Historisch gesehen wurde diese Funktion hinzugefügt, um Textvergleiche und -suchen ohne Berücksichtigung der Groß- und Kleinschreibung zu vereinfachen.

Alternativ könntest du durch jeden Buchstaben im String iterieren und die eingebaute Funktion `Character/toLowerCase` verwenden, um jeden Buchstaben einzeln zu konvertieren. Aber die Verwendung von `clojure.string/lower-case` ist einfacher und übersichtlicher.

Diese Funktion konvertiert Zeichen nach den Regeln für die Standard-Locale. Das bedeutet, dass für einige Sprachen möglicherweise nicht erwartete Ergebnisse auftreten können - zum Beispiel bei Sprachen, die spezielle Zeichen für Großbuchstaben verwenden, die keinen entsprechenden Kleinbuchstaben haben.

## Siehe auch:

- [Offizielle Clojure-Dokumentation zur string/lower-case Funktion](https://clojuredocs.org/clojure.string/lower-case)
- [Eingebaute Java-Methode Character/toLowerCase](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html#toLowerCase(char))