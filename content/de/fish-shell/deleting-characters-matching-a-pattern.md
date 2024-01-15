---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Fish Shell: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum?

Manchmal möchten wir in unserem Code oder in Textdateien bestimmte Zeichen entfernen, um sie lesbarer oder kompakter zu gestalten. Dies kann besonders hilfreich sein, wenn man mit großen Mengen an Daten arbeitet. Mit der aktuellen Version der Fish Shell können wir dieses Problem effektiv lösen.

## Wie geht das?

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir das Befehlsmuster `string delete <pattern>` verwenden. Hier ein Beispiel:

```Fish Shell
set myString "Hello World"
echo $myString
```

Dieser Code erstellt eine Variable `myString` mit dem Wert "Hello World" und gibt ihn aus. Nun möchten wir das Leerzeichen im String löschen, damit "HelloWorld" ausgegeben wird. Dazu fügen wir einfach `string delete " "` hinzu:

```Fish Shell
set myString "Hello World"
echo $myString
set myString (string delete " " $myString)
echo $myString
```

Dies wird den String erfolgreich verändern und "HelloWorld" ausgeben.

## Ausführlicher Einblick

Dieser Befehl verwendet das Muster `string delete`, gefolgt von dem zu löschenden Zeichen oder Muster und der Variable, auf die es angewendet werden soll. Es gibt auch die Möglichkeit, mehrere Zeichen oder Muster gleichzeitig zu löschen, indem sie mit einem Leerzeichen getrennt werden.

Wir können auch einen Bereich von Zeichen löschen, indem wir das Muster `string delete --start <start_index> --end <end_index>` verwenden. Dabei geben wir den Anfangs- und den Endindex des Bereichs an, den wir löschen möchten.

Es gibt auch die Option `--glob`, die es uns ermöglicht, ein globales Muster zu verwenden, um Zeichen zu löschen. Zum Beispiel könnten wir mit `string delete --glob "*.txt" $myString` alle Zeichen löschen, die mit ".txt" enden.

## Weitere Informationen findest du hier

- [Fish Shell Dokumentation zu `string delete`](https://fishshell.com/docs/current/cmds/string.html#string-delete)
- [Offizielles Fish Shell GitHub Repository](https://github.com/fish-shell/fish-shell)
- [Fish Shell User Guide (englisch)](https://fishshell.com/docs/current/index.html)

---
**Siehe auch**

- `string replace` Befehl in der Fish Shell
- `grep -v` Befehl in Bash Shell