---
title:                "Fish Shell: Eine Zeichenkette großschreiben."
simple_title:         "Eine Zeichenkette großschreiben."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Großbuchstaben in einem String kann in der Programmierung hilfreich sein, um bestimmte Teile des Textes hervorzuheben oder die Lesbarkeit zu verbessern. In diesem Blogbeitrag werden wir uns ansehen, wie man in der Fish Shell einen String in Großbuchstaben umwandeln kann.

## Wie geht das?

Um einen String in Großbuchstaben zu konvertieren, können wir die `string`-Funktion `toupper` verwenden.

```
Fish Shell code block:
set my_string "Hallo Welt"
echo (string toupper $my_string)
```

Die Ausgabe dieses Codes wäre `HALLO WELT`, da die `toupper`-Funktion jeden Buchstaben in Großbuchstaben umwandelt.

Wenn wir nur den ersten Buchstaben eines Strings in Großbuchstaben umwandeln möchten, können wir die `string`-Funktion `capitalize` verwenden.

```
Fish Shell code block:
set my_string "hello world"
echo (string capitalize $my_string)
```

Die Ausgabe dieses Codes wäre `Hello world`, da nur der erste Buchstabe in Großbuchstaben umgewandelt wird.

## Tiefergehende Informationen

Die `toupper`- und `capitalize`-Funktionen sind Teil des Fish Shell `string`-Moduls, das eine Vielzahl von nützlichen Funktionen enthält, um Strings zu manipulieren. Diese Funktionen können auch in anderen Situationen nützlich sein, zum Beispiel um Eingaben von Benutzern zu normalisieren oder um Strings für die Ausgabe zu formatieren.

Um mehr über das `string`-Modul und seine Funktionen zu erfahren, kann die Fish Shell Dokumentation konsultiert werden.

## Siehe auch

- Fish Shell string module documentation: https://fishshell.com/docs/current/cmds/string.html
- Offizielle Fish Shell Website: https://fishshell.com/
- Blogbeitrag über Schleifen in der Fish Shell: https://linktoblogpost.com