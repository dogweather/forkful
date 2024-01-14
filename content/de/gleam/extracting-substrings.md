---
title:    "Gleam: Substrings extrahieren"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Die Extraktion von Teilstrings kann eine nützliche Technik sein, wenn Sie bestimmte Informationen aus einem Text oder einer Zeichenfolge erhalten möchten. Zum Beispiel könnten Sie nach bestimmten Schlüsselwörtern in einem Dokument suchen oder eine bestimmte Anzahl von Zeichen aus einer längeren Zeichenfolge extrahieren. In diesem Blog-Beitrag werden wir Ihnen zeigen, wie Sie dies in der Programmiersprache Gleam tun können.

## Wie geht man vor

Um Teilstrings in Gleam zu extrahieren, können Sie die Funktion `String.substring()` verwenden. Hier ist ein Beispielcode, der den Großteil des Alphabets aus einer Zeichenfolge extrahiert:

```Gleam
fn main() {
    let alphabet = "abcdefghijklmnopqrstuvwxyz";
    let extracted = String.substring(alphabet, 0, 10);
    io.println(extracted);
}
```

Dieser Code würde "abcdefghij" ausgeben, da die substring-Funktion die Zeichen von Position 0 bis 10 in der Zeichenfolge extrahiert. Beachten Sie, dass das erste Zeichen den Index 0 hat. Sie können auch einen Bereich von Indizes angeben, um einen Teil der Zeichenfolge zu extrahieren. Zum Beispiel würde `String.substring(alphabet, 5, 15)` die Zeichen "fghijklmnop" ausgeben.

Sie können auch einen negativen Index verwenden, um die Extraktion von der rechten Seite der Zeichenfolge zu beginnen. Zum Beispiel würde `String.substring(alphabet, -5, -1)` die Zeichen "vwxyz" ausgeben. Sie können auch die Länge der Zeichenfolge als zweiten Parameter angeben, um den Teilstring bis zum Ende der Zeichenfolge zu extrahieren.

## Eine nähere Betrachtung

Die `String.substring()` Funktion ist in Gleam ein Teil des `String` Moduls und kann auf Zeichenfolgen beliebiger Länge angewendet werden. Es gibt auch eine ähnliche Funktion für Byte-Strings, `Byte.substring()`. Für weitere Informationen über die verschiedenen Methoden zum Arbeiten mit Zeichenfolgen in Gleam, können Sie die offizielle Dokumentation konsultieren.

## Siehe auch

- [Gleam-Dokumentation für Zeichenketten](https://gleam.run/documentation/stdlib/string/)
- [Gleam-Code-Beispiele](https://github.com/gleam-lang/gleam/blob/master/examples/) zur Verwendung von `String.substring()` und anderen Zeichenketten-Funktionen
- [Einführung in Gleam-Programmierung](https://gleam.run/getting-started/) für Grundlagen und mehr Beispiele mit Gleam