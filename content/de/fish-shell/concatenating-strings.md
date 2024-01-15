---
title:                "Zusammenfügen von Zeichenketten"
html_title:           "Fish Shell: Zusammenfügen von Zeichenketten"
simple_title:         "Zusammenfügen von Zeichenketten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man sich mit der Verkettung von Strings in der Fish Shell beschäftigen sollte. Zum einen ermöglicht es, komplexe Texte oder Befehle aus verschiedenen Variablen zusammenzusetzen, zum anderen kann es die Lesbarkeit und Wiederverwendbarkeit des Codes verbessern.

# Wie geht's?

Um Strings in der Fish Shell zu verketten, wird der Punkt-Operator verwendet. Dieser verbindet zwei oder mehr Strings miteinander. Hier ein Beispiel:

```Fish Shell
set salutation "Hallo"
set name "Anna"
set greeting $salutation", "$name"!"
```

Der obige Code weist dem String "greeting" den Wert "Hallo, Anna!" zu, indem er den Inhalt der Variablen "salutation" und "name" miteinander verbindet. Die Verkettung von Strings ist jedoch nicht auf Variablen beschränkt, sondern kann auch direkt in Befehlen verwendet werden:

```Fish Shell
echo "Der Benutzer "$USER" ist eingeloggt."
```

Dieser Befehl gibt eine Nachricht aus, in der der Name des Benutzers, der gerade eingeloggt ist, angezeigt wird. Beachte, dass zwischen den einzelnen Teilen des Satzes Leerzeichen eingefügt werden müssen, da der Punkt-Operator nur Strings miteinander verbindet, aber keine zusätzlichen Leerzeichen hinzufügt.

# Tiefer in die Materie eintauchen

Man kann nicht nur einzelne Wörter oder Sätze verketten, sondern auch ganze Listen von Strings. Dazu verwendet man den Befehl "string join", der ein Trennzeichen als zusätzlichen Parameter akzeptiert. Hier ein Beispiel:

```Fish Shell
set colors "rot blau grün gelb"
string join ", " $colors
```

Dies gibt den String "rot, blau, grün, gelb" aus, indem die einzelnen Wörter der Variablen "colors" mit dem Trennzeichen ", " verbunden werden.

Weitere Informationen und Beispiele zur Verkettung von Strings in der Fish Shell findest du in der offiziellen Dokumentation unter [https://fishshell.com/docs/current/tutorial.html#tut_string_concatenation](https://fishshell.com/docs/current/tutorial.html#tut_string_concatenation).

# Siehe auch

- [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
- [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)