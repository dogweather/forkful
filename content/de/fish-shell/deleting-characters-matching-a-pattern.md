---
title:    "Fish Shell: Löschen von Zeichen, die einem Muster entsprechen."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Der Fish Shell ist eine leistungsstarke Programmiersprache, die sich hervorragend für die Shell-Programmierung eignet. Beim Arbeiten mit der Fish Shell gibt es eine Vielzahl von Situationen, in denen man möglicherweise Zeichen löschen möchte, die einem bestimmten Muster entsprechen. In diesem Blog-Beitrag werden wir uns genauer damit befassen, warum man solche Aktionen durchführen möchte.

## Wie es funktioniert

Um Zeichen, die einem bestimmten Muster entsprechen, in der Fish Shell zu löschen, gibt es mehrere Möglichkeiten. Eine gängige Methode ist die Verwendung des "string delete" Befehls. Dieser Befehl verwendet das Muster, das wir löschen möchten, und das zu löschende Zeichen. Hier ist ein Beispiel, wie wir den Befehl verwenden können:

```Fish Shell
string delete "H" "Hello World"
```

Das Ergebnis wird sein: "ello World". Wie Sie sehen können, wird der Buchstabe "H" gelöscht und das restliche Wort bleibt erhalten.

Eine weitere Methode ist die Verwendung des "string replace" Befehls. Dieser Befehl verwendet ein ähnliches Format wie der "string delete" Befehl, jedoch ersetzt er das zu löschende Zeichen durch ein anderes Zeichen. Zum Beispiel können wir den Befehl verwenden, um alle Vokale in einem Wort zu löschen:

```Fish Shell
string replace "*?[aeiou]*" "Hello World" ""
```

Das Ergebnis wird sein: "Hll Wrld". Das Sternchen (*) wird verwendet, um jedes beliebige Zeichen zu entsprechen, und das Fragezeichen (?) wird verwendet, um ein einzelnes Zeichen zu entsprechen.

## Tiefergehende Informationen

Es gibt viele weitere Möglichkeiten, Zeichen zu löschen, die einem bestimmten Muster entsprechen, in der Fish Shell. Zum Beispiel können wir den "sed"-Befehl verwenden, um Zeilen in einer Datei zu löschen, die einem bestimmten Muster entsprechen. Oder wir können reguläre Ausdrücke verwenden, um komplexere Muster zu löschen.

Es ist wichtig zu wissen, wie man Zeichen löscht, die einem bestimmten Muster entsprechen, da dies uns helfen kann, unsere Shell-Programme effizienter und effektiver zu gestalten. Es ermöglicht uns auch, Daten schneller und präziser zu verarbeiten.

## Siehe auch

- [Offizielle Fish Shell Website](https://fishshell.com/)
- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Einsteiger-Tutorial für die Fish Shell](https://medium.com/@vgasparyan1995/become-a-ninja-in-bullet-proof-shell-scripting-with-fish-shell-2148b9065929)