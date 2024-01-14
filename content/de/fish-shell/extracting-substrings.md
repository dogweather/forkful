---
title:    "Fish Shell: Substrings extrahieren"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Die Extraktion von Teilzeichenfolgen ist eine häufige Aufgabe in der Programmierung und kann in Fish Shell einfach und effizient durchgeführt werden. Dieser Artikel wird Ihnen zeigen, wie Sie dies mit einigen Beispielen und Codeschnipseln erreichen können.

## Eine Anleitung

Um eine Teilzeichenfolge mit Fish Shell zu extrahieren, müssen Sie die `string sub` -Funktion verwenden. Dieses Beispiel zeigt, wie Sie eine Teilzeichenfolge aus einem Wort extrahieren können:

```
Fish Shell substring Beispiel

set string "Hallo Welt"

echo $string[2,5]

Ergebnis: llo
```

Sie können auch mehrere Teilzeichenfolgen auf einmal extrahieren, indem Sie eine Liste von Indizes angeben:

```
Fish Shell substring Beispiel

set string "It's a beautiful day"

echo $string[1,3][9,11]

Ergebnis: ibeuti
```

## Tiefer eintauchen

Um zu verstehen, wie die `string sub` -Funktion funktioniert, müssen Sie die Indizes verstehen, die Sie angeben. Die erste Zahl gibt den Startindex an und die zweite Zahl gibt an, wie viele Zeichen von diesem Startindex aus extrahiert werden sollen. Beachten Sie, dass das erste Zeichen den Index 1 hat, nicht 0.

Wenn Sie ein `-` vor der zweiten Nummer angeben, wird angezeigt, dass der Index bis zum Ende der Zeichenfolge extrahiert werden soll.

Sie können auch mehrere Zeichenfolgen in einer einzigen Zeile extrahieren, indem Sie sie zwischen `[]` und mit einem `,` trennen.

## Siehe auch

- [Die offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Strings in Fish Shell manipulieren](https://fishshell.com/docs/current/tutorial.html#strings)
- [Das Tutorial zur Fish Shell von OMG! Ubuntu!](https://www.omgubuntu.co.uk/how-to-use-fish-shell-beginners-guide)