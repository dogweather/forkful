---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Fish Shell: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon lange mit dem Fish Shell arbeitest, hast du vielleicht schon einmal von regulären Ausdrücken gehört. Sie sind eine leistungsstarke Möglichkeit, Texte zu durchsuchen und zu manipulieren. Reguläre Ausdrücke können dein Leben als Fish-Shell-Entwickler leichter machen, indem sie dir ermöglichen, effektiver und effizienter zu arbeiten.

## Wie geht's

Für diejenigen, die noch nicht mit regulären Ausdrücken vertraut sind, hier ist eine kurze Einführung. Reguläre Ausdrücke sind Sequenzen von Zeichen, die verwendet werden, um eine bestimmte Suche in einem Text durchzuführen. Im Fish Shell können reguläre Ausdrücke mit dem `grep`-Befehl verwendet werden, der standardmäßig in der Shell vorhanden ist.

```Fish Shell
set liste "Apfel, Birne, Banane"
```

Um zum Beispiel nach allen Früchten zu suchen, die mit dem Buchstaben "B" beginnen, verwende folgenden Befehl:

```Fish Shell
echo $liste | grep "^B"
```

Das Ergebnis wird die Wörter "Birne" und "Banane" enthalten, da das "^" Zeichen den Anfang eines Wortes anzeigt. Hier sind einige weitere nützliche Regeln für reguläre Ausdrücke:

- `.` steht für ein beliebiges einzelnes Zeichen
- `*` steht für kein oder mehrere Vorkommen des vorherigen Zeichens
- `+` steht für eins oder mehrere Vorkommen des vorherigen Zeichens
- `$` stellt das Ende eines Wortes dar

Probier es selbst aus und experimentiere mit verschiedenen regulären Ausdrücken. Du wirst schnell feststellen, wie mächtig sie sind!

## Tief eintauchen

Wenn du ein tieferes Verständnis für reguläre Ausdrücke hast und mehr darüber erfahren möchtest, wie du sie in deiner Fish-Shell-Entwicklung nutzen kannst, gibt es eine Fülle von Ressourcen online. Eine gute Möglichkeit, deine Fähigkeiten zu verbessern, ist es, mit verschiedenen regulären Ausdrücken zu experimentieren, bis du dich wohl fühlst und die Konzepte vollständig verstanden hast.

Eine weitere hilfreiche Ressource ist die offizielle Fish-Shell-Dokumentation, die eine umfassende Anleitung zu regulären Ausdrücken und ihrer Verwendung in der Shell bietet.

## Siehe auch

- [Reguläre Ausdrücke für Anfänger](https://www.regular-expressions.info/tutorial.html)
- [Offizielle Fish-Shell-Dokumentation](https://fishshell.com/docs/current/index.html)
- [Cheatsheet für reguläre Ausdrücke in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-cheatsheet)