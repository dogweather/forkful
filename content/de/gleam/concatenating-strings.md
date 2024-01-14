---
title:    "Gleam: Zeichenfolgen verketten"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Warum
Warum sollte man sich überhaupt mit der Verknüpfung von Zeichenketten beschäftigen? Nun, wenn Sie in der Programmierung tätig sind, werden Sie oft auf Daten stoßen, die aus mehreren Teilen bestehen und die Sie zu einer einzigen Zeichenkette zusammenführen müssen. Mit der Konkatenation von Zeichenketten können Sie diese Aufgabe effizient und zuverlässig lösen.

# Wie man es macht
Die Konkatenation von Zeichenketten in Gleam ist einfach, aber leistungsstark. Schauen wir uns einige Beispiele an, um zu sehen, wie es funktioniert.

```Gleam
let name = "Max"
let greeting = "Hallo " ++ name
```
Das Ergebnis ist eine neue Zeichenkette, die "Hallo Max" lautet. Beachten Sie, dass wir den `++`-Operator verwendet haben, um die beiden Zeichenketten zu verbinden.

Wir können auch mehr als zwei Zeichenketten kombinieren:

```Gleam
let first = "Hallo"
let middle = ", ich bin "
let last = "Max"
let greeting = first ++ middle ++ last
```
Das Ergebnis ist wiederum "Hallo, ich bin Max". Einfach, oder?

# Tief eintauchen
Wenn Sie genauer hinschauen, ist die Konkatenation von Zeichenketten in Gleam sehr effizient. Im Hintergrund verwendet der `++`-Operator das `String.concat`-Modul, das eine kontinuierliche Verkettung von Zeichenketten ermöglicht, ohne unnötige Zwischenspeicherung oder Kopiervorgänge durchzuführen. Das macht es zu einer effizienten und performanten Option.

Es ist auch erwähnenswert, dass Gleam Unicode-Unterstützung bietet, sodass Sie problemlos Zeichenketten mit Sonderzeichen und Emojis verknüpfen können.

# Siehe auch
- Offizielle Gleam-Dokumentation zu Zeichenketten: https://gleam.run/book/standard-library#strings
- Ein Einführungsvideo zur Konkatenation von Zeichenketten in Gleam: https://www.youtube.com/watch?v=t1ykjkCcAoU
- Beispielcode für die Konkatenation von Zeichenketten: https://github.com/example-code/gleam/