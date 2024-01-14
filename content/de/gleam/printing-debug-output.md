---
title:    "Gleam: Ausgabe von Debugging-Informationen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Warum

Debugging ist ein wichtiger Teil der Softwareentwicklung und das Drucken von Debug-Ausgaben ist eine nützliche Methode, um Probleme in Ihrem Code zu identifizieren. In diesem Blog-Beitrag erfahren Sie, warum es wichtig ist, Debug-Ausgaben zu drucken und wie Sie dies mit Gleam erreichen können.

# Wie geht das

Um Debug-Ausgaben in Gleam zu drucken, können Sie die `info/1` Funktion aus der standardmäßigen `log`-Bibliothek verwenden. Diese Funktion akzeptiert einen beliebigen Datentyp als Argument und gibt ihn auf der Konsole aus. Hier ist ein Beispiel, wie Sie die `info/1` Funktion verwenden können:

```Gleam
import gleam/debug

fn main() {
  let name = "Max"
  debug.info("Der Name ist", name)
}
```

Die Ausgabe dieses Codes sieht so aus:

```
Der Name ist Max
```

Sie können auch komplexe Datenstrukturen wie Listen oder Records an die `info/1` Funktion übergeben. Die Ausgabe wird automatisch formatiert, um diese Datenstrukturen besser lesbar zu machen. Hier ist ein Beispiel:

```Gleam
import gleam/debug

fn main() {
  let numbers = [1, 2, 3, 4]
  let person = {name: "Anna", age: 25}
  debug.info("Zahlen:", numbers)
  debug.info("Person:", person)
}
```

Die Ausgabe dieses Codes sieht so aus:

```
Zahlen: [1, 2, 3, 4]
Person: {name: "Anna", age: 25}
```

# Tiefere Einblicke

Das Drucken von Debug-Ausgaben kann Ihnen helfen, herauszufinden, wo genau in Ihrem Code ein Problem liegt. Es ist besonders nützlich, wenn Sie komplexe Funktionen oder Datenstrukturen haben. Sie können auch die `debug`-Bibliothek verwenden, um informative Fehlermeldungen zu generieren. Hier ist ein Beispiel:

```Gleam
import gleam/debug

fn divide(a, b) {
  case b {
    0 -> debug.panic("Division durch 0 nicht erlaubt")
    _ -> a / b
  }
}
```

Wenn `b` 0 ist, wird die folgende Fehlermeldung gedruckt:

```
✗ Division durch 0 nicht erlaubt
```

Das Drucken von Debug-Ausgaben ist auch hilfreich, um die Reihenfolge, in der Ihr Code ausgeführt wird, zu verstehen. Manchmal kann ein Fehler auftreten, weil eine Funktion in einem anderen Kontext aufgerufen wird als erwartet. Der Einsatz von `debug.info` an verschiedenen Stellen in Ihrem Code kann Ihnen dabei helfen, dieses Problem zu erkennen.

# Siehe auch

- [Gleam Dokumentation über die `debug`-Bibliothek](https://gleam.run/book/stdlib.html#debug)
- [Tutorial zur Verwendung von Gleam für Debugging](https://medium.com/@hogehoge/debugging-gleam-code-e1df7b2704db)
- [Vergleich von Methoden zur Fehlersuche in Gleam](https://www.gleam-lang.org/posts/comparing-debugging-methods-in-gleam/)