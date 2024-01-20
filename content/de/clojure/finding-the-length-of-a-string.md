---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Länge eines Strings in Clojure ermitteln

## Was & Warum?

Die Länge eines Strings zu ermitteln, bedeutet herauszufinden, wie viele Zeichen der String enthält. Dies wird häufig benötigt, um Dateneingaben zu überprüfen oder die Datenverarbeitung zu steuern.

## So geht's:

Clojure stellt die eingebaute Funktion `count` bereit, die die Länge eines Strings ermitteln kann.

```Clojure 
(defn string-length [s]
  (count s))

(println (string-length "Hallo Welt"))
```

Beispiel:

```Clojure 
user=> (defn string-length [s] (count s))
#'user/string-length
user=> (string-length "Hallo Welt")
10
```

## Vertiefung

Die Funktion `count` wurde in Clojure 1.0 eingeführt und gibt die Anzahl der Elemente einer Kollektion zurück. In unserem Fall ist die Kollektion ein String, aber `count` kann auch für Listen, Vektoren und andere Kollektionen verwendet werden.

Die Verwendung von `count` ist eine einfache und effiziente Methode, um die Länge eines Strings zu ermitteln, es gibt jedoch auch alternative Methoden. Eine davon ist die Verwendung der Java-Methode `length`:

```Clojure 
(.length "Hallo Welt")
```
 
Dies führt die native Java-Methode auf den String aus und gibt auch in diesem Fall die Anzahl der Zeichen zurück. Beide Implementierungen sind im Allgemeinen gleichwertig, obwohl die Nutzung der nativen Java-Methode für umfangreiche Operationen möglicherweise effizienter ist.

## Siehe auch

Weitere Informationen und zusätzliche Beispiele finden Sie unter:
- Clojure-Dokumentation zu `count`: [link](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)
- Clojure-Dokumentation zur Interaktion mit Java: [link](https://clojure.org/reference/java_interop)