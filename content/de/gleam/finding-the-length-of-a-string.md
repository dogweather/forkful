---
title:    "Gleam: Die Länge eines Strings ermitteln"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

German translation:

## Warum

Die Bestimmung der Länge einer Zeichenkette kann in verschiedenen Programmiersituationen äußerst nützlich sein. Es ermöglicht uns, die Anzahl der Zeichen in einer Zeichenkette zu ermitteln, was zu einer effizienteren Bearbeitung und Verarbeitung von Daten führen kann.

## Wie man die Länge einer Zeichenkette bestimmt

Um die Länge einer Zeichenkette in Gleam zu bestimmen, können Sie die integrierte Funktion `String.length()` verwenden. Hier ist ein Beispielcode, wie Sie es verwenden können:

```Gleam
fn main() {
  let str = "Hallo Welt";
  let len = String.length(str);
  IO.print("Die Länge der Zeichenkette beträgt: ");
  IO.print(len);
}
```

**Output:**
```
Die Länge der Zeichenkette beträgt: 10
```

Sie können auch die Länge von Zeichenketten mit Sonderzeichen bestimmen. Hier ist ein Beispielcode:

```Gleam
fn main() {
  let str = "Ich ♥ Gleam";
  let len = String.length(str);
  IO.print("Die Länge der Zeichenkette beträgt: ");
  IO.print(len);
}
```

**Output:**
```
Die Länge der Zeichenkette beträgt: 11
```

## Tiefgehende Informationen zur Bestimmung der Länge einer Zeichenkette

Bevor Sie mit der Bestimmung der Länge einer Zeichenkette in Gleam beginnen, ist es wichtig zu verstehen, dass die Funktion `String.length()` nur die Anzahl der Unicode-Zeichen in einer Zeichenkette zählt. Sie berücksichtigt nicht die Anzahl der Bytes, die in der Zeichenkette enthalten sind.

Zum Beispiel wird die Länge der Zeichenkette "äöü" als 3 gezählt, obwohl sie aus 6 Bytes besteht (2 Bytes pro Zeichen). Dies liegt daran, dass Gleam standardmäßig Unicode-Strings verwendet, um die Unterstützung für internationale Zeichen zu gewährleisten.

Wenn Sie die Anzahl der Bytes in einer Zeichenkette bestimmen möchten, können Sie die Funktion `Binary.length()` verwenden, da Gleam Unicode-Zeichen standardmäßig in Binärdaten speichert.

## Siehe auch

- [Gleam-Dokumentation zur String-Datenstruktur](https://gleam.run/book/stdlib#string)
- [Offizielle Gleam-Website](https://gleam.run/)