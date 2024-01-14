---
title:                "Clojure: Extrahieren von Teichen"
simple_title:         "Extrahieren von Teichen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings kann nützlich sein, wenn Sie einen Text bearbeiten möchten und nur bestimmte Abschnitte davon benötigen.

## Wie geht das?

Das Extrahieren von Teilstrings ist in Clojure sehr einfach. Sie können die `subs`-Funktion verwenden, um einen Teil eines Strings basierend auf einer gegebenen Startposition und Länge zu extrahieren.

Ein Beispiel:

```Clojure
(def text "Hallo, wie geht es dir?")

(subs text 0 5)
```

Das obige Beispiel würde den Teilstring "Hallo" aus dem ursprünglichen Text extrahieren und folgende Ausgabe liefern:

`Hallo`

Sie können auch die `subs`-Funktion mit negativen Indizes verwenden, um Teilstrings vom Ende des Textes aus zu extrahieren. Zum Beispiel, um die letzten 3 Zeichen des Textes auszugeben, könnten Sie Folgendes tun:

```Clojure
(subs text -3)
```

Die Ausgabe wäre dann:

`dir`

## Tiefer gehende Informationen

Sie können auch einen Schritt weiter gehen und Teilstrings basierend auf regulären Ausdrücken extrahieren. Hierfür können Sie die `re-find`-Funktion verwenden, die einen regulären Ausdruck auf einen String anwendet und den ersten Treffer als Teilstring zurückgibt.

Ein Beispiel:

```Clojure
(def text "Ich liebe Clojure!")

(re-find #"[a-z]+" text)
```

Dies würde den ersten Teilstring aus dem ursprünglichen String extrahieren, der aus mindestens einem Kleinbuchstaben besteht, und folgende Ausgabe liefern:

`ich`

## Siehe auch

- [Clojure Dokumentation zu `subs`](https://clojuredocs.org/clojure.core/subs)
- [Clojure Dokumentation zu `re-find`](https://clojuredocs.org/clojure.core/re-find)
- [Reguläre Ausdrücke in Clojure](https://clojure.org/reference/regular_expressions)