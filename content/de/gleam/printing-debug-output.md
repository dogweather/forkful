---
title:    "Gleam: Ausgabe von Debug-Meldungen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Das Drucken von Debug-Ausgaben ist ein wichtiges Tool für die Fehlersuche und das Verständnis des Codes beim Programmieren in Gleam. Es kann helfen, Fehler schneller zu finden und die Funktionsweise des Codes besser zu verstehen.

# Wie geht man vor

Um Debug-Ausgaben in Gleam zu drucken, nutzen wir die Funktion `gleam_io:p/1`, die einen beliebigen Wert in der Konsole ausgibt. Hier ist ein Beispiel, wie wir diese Funktion nutzen können:

```Gleam
import gleam/io

pub fn main() {
  greeting = "Hallo Welt"
  gleam_io:p(greeting)
}
```

Dieser Code würde die Debug-Ausgabe "Hallo Welt" in der Konsole ausgeben. Wir können auch komplexe Datentypen wie Lists oder Tuples ausdrucken, um den Inhalt von Variablen besser zu verstehen.

```Gleam
import gleam/io

pub fn main() {
  numbers = [1, 2, 3]
  gleam_io:p(numbers)
}

```

Die Ausgabe wäre `[1, 2, 3]`, was uns zeigt, dass `numbers` eine Liste mit den Zahlen 1, 2 und 3 ist. Dies kann besonders nützlich sein, wenn wir mit komplexeren Datenstrukturen arbeiten und den Überblick behalten wollen.

# Tiefere Einblicke

Wenn wir Debug-Ausgaben nutzen, sollten wir sicherstellen, dass sie nicht in der finalen Version unseres Codes enthalten sind. Wir können sie zum Beispiel mit `if`-Statements umgeben, um sicherzustellen, dass sie nur ausgeführt werden, wenn wir uns in der Debug-Phase befinden.

```Gleam
import gleam/io

pub fn main() {
  debug = true
  if debug {
    // Code hier
  }
}
```

Wir können auch mehrere Werte in einer Debug-Ausgabe ausdrucken, indem wir sie mit einem Komma trennen.

```Gleam
import gleam/io

pub fn main() {
  first_name = "Max"
  last_name = "Mustermann"
  gleam_io:p(first_name, last_name)
}
```

Die Ausgabe wäre `Max, Mustermann`, was uns den Wert von `first_name` und `last_name` zeigt.

# Siehe auch

-[Gleam Dokumentation](https://gleam.run/documentation.html)
-[Gleam Beispiele und Tutorials](https://github.com/gleam-lang/gleam/tree/master/examples)