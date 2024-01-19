---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Interpolation bedeutet das Einsetzen von Variablen- oder Ausdruckswerten in eine Zeichenkette. Es erleichtert die Formatierung und die Manipulation von Strings.

## So geht's:

Clojure unterstützt in seiner Standardbibliothek keine String-Interpolation, aber es gibt nützliche Funktionen wie `format` und Bibliotheken wie `strfmt`.

### Verwendung von `format`:

```clojure
(let [name "Hans" age 25]
  (format "Hallo, ich bin %s und ich bin %d Jahre alt." name age))
```

Ausgabe:

```clojure
"Hallo, ich bin Hans und ich bin 25 Jahre alt."
```

### Verwendung von `strfmt`:

Great Um `strfmt` zu benützen, müssen wir es zu unserem Projekt hinzufügen.

In der `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.1"]
               [yogthos/strfmt "0.1.1"]]
```

In der Code Datei:

```clojure
(require '[yogthos.strfmt :refer [fmt]])

(let [name "Hans" age 25]
  (fmt "${name}, du bist ${age} Jahre alt." {:name name :age age}))
```

Ausgabe:

```clojure
"Hans, du bist 25 Jahre alt."
```

## Vertiefung

Historisch gesehen verlässt sich Clojure auf Java String-Methoden und -Funktionen, da es sich um eine Host-Sprache zur JVM handelt. Java selbst unterstützt keine String-Interpolation, hat aber `String.format()`, was die Inspirationsquelle für Clojures `format` war.

Alternativen zur String-Interpolation in Clojure sind das manuelle Verketten von Strings mit der `str` Funktion oder das Verwenden anderer Bibliotheken wie `hiccup` für HTML-Ausgabe dies können weiterhin effizient und einfach zu handhaben sein. 

Die Implementationsdetails der beiden vorgestellten Methoden unterscheiden sich. `format` verwendet die `java.util.Formatter` Klasse, während `strfmt` einen eigenen Lexer/Parsen zum Analysieren und Verarbeiten von Templates hat.

## Siehe auch

Check out die folgenden Links für zusätzliche Informationen und Tutorials:

1. Clojure `format` Funktion Dokumentation: https://clojuredocs.org/clojure.core/format
2. Die strfmt Bibliothek auf GitHub: https://github.com/yogthos/strfmt
3. Java `String.format()` Methode Dokumentation: https://docs.oracle.com/javase/7/docs/api/java/util/Formatter.html