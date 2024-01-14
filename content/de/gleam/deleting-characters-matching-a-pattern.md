---
title:    "Gleam: Löschen von Zeichen, die einem Muster entsprechen."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

**## Warum**

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Methode, um bestimmte Daten aus einer Zeichenkette zu entfernen. Zum Beispiel kann es verwendet werden, um bestimmte Sonderzeichen oder Buchstaben aus einem Text zu entfernen, um ihn sauberer oder besser lesbar zu machen.

**## Wie geht es**

Um Zeichen in Gleam zu löschen, können Sie die Funktion `String.delete/2` verwenden. Sie erwartet zwei Argumente: die Zeichenkette, aus der die Zeichen gelöscht werden sollen, und das Muster, nach dem sie suchen soll.

```
Gleam>String.delete("Hallo Welt", "l")
"Hao Wett"
```

In diesem Beispiel werden alle Buchstaben "l" aus der Zeichenkette "Hallo Welt" gelöscht, was zu "Hao Wett" führt.

Alternativ können Sie ein reguläres Ausdrucksmuster verwenden, um bestimmte Zeichen zu löschen.

```
Gleam>String.delete("Hallo Welt!", "[ ,!]")
"HalloWelt"
```

Hier werden alle Leerzeichen, Kommata und Ausrufezeichen aus der Zeichenkette entfernt.

**## Tiefer eintauchen**

Um tiefer in das Löschen von Zeichen in Gleam einzutauchen, sollten Sie das Modul `Gleam.String` genauer unter die Lupe nehmen. Hier finden Sie weitere nützliche Funktionen, um das Entfernen von Zeichen zu erleichtern, wie zum Beispiel `String.delete_all/2`, `String.replace/3` und `String.filter/2`.

Es kann auch hilfreich sein, sich mit regulären Ausdrücken und deren Syntax vertraut zu machen, um noch präzisere Muster zum Löschen von Zeichen zu erstellen. Es gibt viele Ressourcen online, die dabei helfen können, wie z.B. [Regular-Expressions.info](https://www.regular-expressions.info/).

**## Siehe auch**

- [Gleam.String-Dokumentation](https://hexdocs.pm/gleam/gleam.String.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)