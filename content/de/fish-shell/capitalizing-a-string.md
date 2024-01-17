---
title:                "Eine Zeichenfolge großschreiben."
html_title:           "Fish Shell: Eine Zeichenfolge großschreiben."
simple_title:         "Eine Zeichenfolge großschreiben."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was ist Capitalizing und warum machen Programmierer es?

Capitalizing bezieht sich auf die Umwandlung eines Strings, um den ersten Buchstaben jedes Wortes zu groß zu schreiben. Programmierer machen dies, um Strings lesbarer zu machen oder um bestimmte Programmierungsanforderungen zu erfüllen.

## So gehts:

Eine Möglichkeit, einen String in Fish Shell zu capitalizen, ist die Verwendung des `string uppercase` Befehls. Zum Beispiel:

```fish
string uppercase "hallo welt"
```

Dieser Befehl gibt `Hallo Welt` als Output zurück.

Eine andere Möglichkeit ist die Verwendung von regulären Ausdrücken mit dem ```sed``` Befehl, um die einzelnen Wörter im String zu ändern. Zum Beispiel:

```fish
echo "hallo welt" | sed 's/^./\U&/g'
```

Dieser Befehl gibt ebenfalls `Hallo Welt` als Output zurück.

## Tiefergehende Informationen:

Capitalizing ist keine exklusive Funktion von Fish Shell und kann auch in anderen Programmiersprachen und Shells erreicht werden. In vielen Programmiersprachen gibt es eingebaute Funktionen wie `toUpperCase()` oder `capitalize()`.

Alternativ können auch Skripte oder Funktionen in Fish Shell erstellt werden, um Strings zu capitalizen. Dies kann nützlich sein, wenn der String nicht als direktes Argument an den Befehl übergeben werden kann.

## Siehe auch:

- [Fish Shell Dokumentation zur `string` Befehlsgruppe](https://fishshell.com/docs/current/cmds.html#string)
- [StackOverflow Diskussion über die Verwendung von `sed` zum Capitalizen von Strings](https://stackoverflow.com/questions/10456538/how-to-mass-uppercase-but-keep-some-specific-characters-lowercase-using-sed)