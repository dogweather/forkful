---
title:    "Fish Shell: Zeichen löschen, die einem Muster entsprechen."
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Technik in der Fish Shell Programmierung. Mit dieser Funktion können Sie unerwünschte Zeichen in einer Datei oder einem String effizient entfernen.

## Wie man vorgeht

Um Zeichen in der Fish Shell zu löschen, verwenden Sie das Befehlszeilentool "sed" in Kombination mit regulären Ausdrücken. Der folgende Code zeigt, wie Sie alle Leerzeichen aus einem String entfernen können:

```Fish Shell
set str 'Fisch ist cool'
set str (echo $str | sed 's/ //g')
echo $str
```
Output: `Fischtisull`

In diesem Beispiel verwenden wir den Befehl "sed" mit dem Flag "s", das für "substitution" (Ersetzung) steht. Wir geben ein Leerzeichen und einen Schrägstrich ("/ ") an, um anzugeben, welches Zeichen wir ersetzen möchten. Nach dem zweiten Schrägstrich geben wir an, durch welches Zeichen wir das vorherige ersetzen wollen, in diesem Fall durch keinen Wert (" //"). Das Flag "g" steht für "global" und gibt an, dass alle übereinstimmenden Zeichen im String ersetzt werden sollen.

## Tiefere Einblicke

Die Verwendung von regulären Ausdrücken in der Fish Shell bietet eine leistungsstarke Möglichkeit, Zeichenmuster zu identifizieren und zu bearbeiten. Hier sind einige der gängigsten Syntax-Elemente, die Sie bei der Arbeit mit regulären Ausdrücken verwenden können:

- `.` steht für ein beliebiges Zeichen
- `[]` gibt eine Liste von Zeichen an, die übereinstimmen sollen
- `*` steht für Null oder mehr Wiederholungen des vorherigen Zeichens
- `+` steht für eine oder mehr Wiederholungen des vorherigen Zeichens
- `?` steht für Null oder eine Wiederholung des vorherigen Zeichens

Es gibt noch viele weitere Möglichkeiten, reguläre Ausdrücke zu verwenden, um bestimmte Zeichenmuster zu finden oder zu ersetzen. Wir empfehlen Ihnen, weitere Tutorials zu diesem Thema zu lesen, um Ihr Verständnis zu vertiefen.

## Siehe auch

- [Fish Shell Dokumentation über reguläre Ausdrücke](https://fishshell.com/docs/current/index.html#regular-expressions)
- [Tutorial zu regulären Ausdrücken in der Fish Shell](https://dev.to/hirmencedigimahi/fishing-for-patterns-using-regular-expressions-with-fish-shell-40l2)
- [Weitere Beispiele für die Verwendung von regulären Ausdrücken in der Fish Shell](https://dev.to/d_danilm/fish-shell-regular-expressions-1hjm)