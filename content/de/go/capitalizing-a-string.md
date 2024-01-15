---
title:                "String großschreiben"
html_title:           "Go: String großschreiben"
simple_title:         "String großschreiben"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum

Du hast dich entschieden, Go zu lernen und möchtest jetzt lernen, wie du Strings in Großbuchstaben umwandeln kannst? In diesem Artikel werde ich dir zeigen, wie du das ganz einfach machen kannst und warum es nützlich sein kann.

# Wie es geht

In Go gibt es keine eingebaute Funktion für die Großschreibung von Strings, aber wir können die `strings` Bibliothek verwenden, um das zu erreichen. Hier ist ein Beispielcode, wie man einen String in Großbuchstaben umwandeln kann:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "ich bin ein string"
    s = strings.ToUpper(s)
    // Ergebnis: "ICH BIN EIN STRING"
    fmt.Println(s)
}
```

Wie du sehen kannst, können wir die `ToUpper()` Funktion der `strings` Bibliothek verwenden, um den String in Großbuchstaben zu konvertieren. Hier ist noch ein Beispiel, bei dem wir das erste Zeichen eines Strings in Großbuchstaben umwandeln:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "ich bin ein string"
    s = strings.Title(s)
    // Ergebnis: "Ich Bin Ein String"
    fmt.Println(s)
}
```

In diesem Beispiel verwenden wir die `Title()` Funktion, um das erste Zeichen jedes Worts in Großbuchstaben zu setzen.

# Ausführliche Erläuterung

Vielleicht fragst du dich, warum wir überhaupt Strings in Großbuchstaben umwandeln sollten. Es gibt verschiedene Anwendungen für diese Funktionalität, zum Beispiel:

- Formatierung von Nutzereingaben: Wenn du zum Beispiel eine Suchfunktion in deiner Anwendung hast, kannst du sicherstellen, dass unabhängig von der Eingabe des Nutzers, die Groß- und Kleinschreibung keine Rolle spielt.

- Datenbereinigung: Es kann vorkommen, dass Daten aus verschiedenen Quellen in verschiedenen Formaten vorliegen. Wenn du sicherstellen willst, dass sie alle in der gleichen Form sind, kannst du sie in Großbuchstaben umwandeln, um Konsistenz zu erhalten.

- Präsentation von Daten: In manchen Fällen kann es auch nützlich sein, bestimmte Teile eines Strings in Großbuchstaben darzustellen, zum Beispiel für Überschriften oder Betonung.

Ein häufiger Fehler ist es, `s.ToUpper()` zu verwenden. Das funktioniert zwar, jedoch wird dadurch eine Kopie des Strings erstellt und der original String bleibt unverändert. Daher ist es besser, den Wert der `ToUpper()` oder `Title()` Funktion der vorhandenen Variable zuzuweisen.

# Siehe auch

- Offizielle Go Dokumentation über die `strings` Bibliothek: https://golang.org/pkg/strings/
- Beispielcode für die Verwendung von `ToUpper()` und `Title()`: https://gobyexample.com/string-functions