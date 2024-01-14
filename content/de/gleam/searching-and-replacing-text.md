---
title:    "Gleam: Suchen und Ersetzen von Text"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn du ein Programmierer bist, der mit Texten arbeitet, stößt du oft auf die Notwendigkeit, bestimmte Wörter oder Ausdrücke in deinem Code zu finden und zu ersetzen. Dies kann aus verschiedenen Gründen erforderlich sein, zum Beispiel um Fehler zu beheben oder um deinen Code effizienter zu machen. Hier kommt die Gleam-Programmiersprache ins Spiel, die eine robuste Such- und Ersetzungsfunktion bietet, die dir dabei hilft, diese Aufgabe schnell und effektiv zu erledigen.

## Wie geht das?

Die Gleam-Syntax für die Suche und Ersetzung von Text verwendet Mustererkennung und reguläre Ausdrücke, um die entsprechenden Textpassagen zu identifizieren. Hier ist ein Beispiel, um alle Vorkommen von "Gleam" in einem Text zu ersetzen:

```Gleam
match text {
    "Gleam" => "Programmieren"
    text -> text
}
```

In diesem Beispiel haben wir das Muster "Gleam" definiert und ihm den Ersatzwert "Programmieren" zugewiesen. Die Variante `text -> text` stellt sicher, dass alle anderen Passagen im Text unverändert bleiben. Das Ergebnis dieses Codes sind alle Vorkommen von "Gleam", die jetzt mit "Programmieren" ersetzt werden.

Ein weiteres wichtiges Merkmal der Gleam-Syntax ist die Möglichkeit, reguläre Ausdrücke zu verwenden. Dies ist besonders nützlich, wenn du nach bestimmten Mustern suchen und ersetzen möchtest. Hier ist ein Beispiel, um alle Vorkommen von Zahlen in einem Text zu ersetzen:

```Gleam
import gleam/regexp

match text {
    regexp.match([0-9]+) -> "Zahl"
    text -> text
}
```

In diesem Beispiel importieren wir das Regexp-Modul von Gleam und verwenden es, um alle zusammenhängenden Zahlen (es können auch längere Zahlen sein) zu identifizieren und durch den Ersatzwert "Zahl" zu ersetzen.

## Tiefere Einblicke

Die Gleam-Suche und -Ersetzungsfunktion bietet auch erweiterte Optionen, wie zum Beispiel die Möglichkeit, die Groß- und Kleinschreibung zu ignorieren oder einen globalen Austausch durchzuführen. Sie bietet auch verschiedene Funktionen, um die Such- und Ersetzungslogik anzupassen.

Wenn du weitere Informationen und Beispiele zum Suchen und Ersetzen in Gleam benötigst, schaue dir die offizielle Dokumentation an.

## Siehe auch

- Gleam offizielle Dokumentation: https://gleam.run/
- Reguläre Ausdrücke in Gleam: https://gleam.run/articles/regular-expressions-in-gleam/