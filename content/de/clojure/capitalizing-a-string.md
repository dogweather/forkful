---
title:                "Clojure: Verkettung eines Strings"
simple_title:         "Verkettung eines Strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum

Die Funktion `capitalize` in Clojure wird verwendet, um einen String in Großbuchstaben zu konvertieren. Dies kann hilfreich sein, wenn man zum Beispiel einen Namen oder einen Titel in einem Satz hervorheben möchte.

# Wie es geht

Die Verwendung von `capitalize` ist sehr einfach. Zunächst müssen wir jedoch das `clojure.string`-Modul importieren, da sich diese Funktion in diesem Modul befindet.

```Clojure
(require '[clojure.string :as str])
```

Nun können wir die Funktion `capitalize` verwenden, indem wir den zu konvertierenden String als Argument übergeben.

```Clojure
(str/capitalize "ich bin ein String")
```

Das Ausgaberesultat ist "Ich bin ein String". Falls wir jedoch nur den ersten Buchstaben eines Satzes groß schreiben wollen, können wir die Funktion `capitalize-first` verwenden.

```Clojure
(str/capitalize-first "ich bin ein Satz.")
```

Das Ausgaberesultat ist "Ich bin ein Satz." Dies kann hilfreich sein, wenn wir einen String in ein korrektes Format bringen wollen, bevor wir ihn beispielsweise in einer Datenbank speichern.

# Tieferer Einblick

Die `capitalize`-Funktion macht sich intern eine Verwaltung von Unicode-Text zu Nutze. Sie nutzt die Funktion `Character/isLowerCase` um zu überprüfen, ob ein Buchstaben ein Kleinbuchstabe ist und wendet daraufhin die entsprechende Konvertierung an. Diese Funktion ermöglicht somit auch die Konvertierung von Nicht-ASCII Buchstaben.

# Siehe auch

- [Offizielle Dokumentation für `clojure.string`](https://clojuredocs.org/clojure.string)
- [Unicode-Zeichen in Clojure](https://clojure.org/reference/unicode)