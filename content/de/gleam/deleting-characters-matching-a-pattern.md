---
title:                "Gleam: Löschen von Zeichen, die einem Muster entsprechen."
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist ein häufig verwendetes Verfahren in der Programmierung, um unerwünschte Zeichen automatisch zu entfernen und die Daten bereinigt zu halten.

## Wie geht man vor?

Zunächst müssen wir in Gleam ein geeignetes Modul importieren:

```Gleam
import regex
```

Als nächstes definieren wir das Pattern, nach dem wir suchen wollen, und speichern es in einer Variable:

```Gleam
let pattern = "{:digit:}"
```

In diesem Fall entspricht das Pattern allen numerischen Zeichen.

Nun können wir eine Funktion erstellen, die das Löschen der Zeichen durchführt:

```Gleam
fn delete_characters(string) {
  let matches = regex.find_all(pattern, string)
  let new_string = string
  for match in matches {
    new_string = regex.replace_all(match, "", new_string)
  }
  new_string
}
```

Hier nutzen wir die Funktion `find_all` aus dem importierten Modul `regex`, um alle Zeichen zu finden, die dem Pattern entsprechen. Danach durchlaufen wir jede Übereinstimmung und nutzen die Funktion `replace_all`, um die Zeichen durch leere Strings zu ersetzen. Schließlich geben wir den bereinigten String zurück.

Um zu sehen, wie die Funktion in der Praxis funktioniert, können wir sie auf einen beliebigen String anwenden und den output ausgeben:

```Gleam
let my_string = "Hal1l2o 3W4e5l6t7"
delete_characters(my_string)

>>> "Hallo Welt"
```

## Tiefergehende Infos

Das Löschen von Zeichen kann auch in komplexeren Szenarien hilfreich sein. Zum Beispiel könnten wir ein Pattern definieren, das auf alle Leerzeichen, Tabs und Zeilenumbrüche reagiert und somit einen String bereinigt.

Außerdem kann es in manchen Fällen hilfreich sein, die gelöschten Zeichen durch andere zu ersetzen, anstatt sie einfach zu entfernen. Dafür können wir die Funktion `replace_all` entsprechend anpassen.

In der Gleam-Dokumentation sind weitere Beispiele und Funktionen für das Löschen von Zeichen mit Pattern zu finden.

## Siehe auch

- https://gleam.run/modules/regex#find_all
- https://gleam.run/modules/regex#replace_all
- https://gleam.run/docs/regular-expressions