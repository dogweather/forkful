---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Fish Shell: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Manchmal muss man in der Programmierung bestimmte Zeichenfolgen (auch als Strings bekannt) in Kleinbuchstaben umwandeln. Dies kann aus verschiedenen Gründen notwendig sein, zum Beispiel um Vergleiche zwischen Strings durchzuführen oder um eine konsistente Darstellung von Texten zu gewährleisten.

## Wie geht das?

Das Umwandeln einer Zeichenfolge in Kleinbuchstaben ist mit Fish Shell ziemlich einfach. Man muss lediglich den Befehl `string tolower` verwenden und die zu konvertierende Zeichenfolge als Argument angeben. Hier ist ein Beispiel:

```Fish Shell
set meine_string "Hallo WELT"
string tolower $meine_string
```

Die Ausgabe wäre `hallo welt`, da alle Buchstaben in Kleinbuchstaben umgewandelt wurden. Beachte, dass der Original-String unverändert bleibt, es sei denn, man weist die Ausgabe dem String erneut zu.

## Tiefergehende Erklärung

Das Umwandeln von Zeichenfolgen in Kleinbuchstaben ist tatsächlich ein etwas komplexerer Vorgang als man zunächst denken mag. Viele Sprachen haben unterschiedliche Konventionen für die Groß- und Kleinschreibung, die sich auf die Umwandlung auswirken können. Zum Beispiel wenn es ums Sortieren von Strings geht, kann die Verwendung von Kleinbuchstaben zu unerwarteten Ergebnissen führen. Fish Shell verwendet das Unicode-Zeichen "Lowercase Map" für die Umwandlung, um sicherzustellen, dass auch Sonderzeichen korrekt behandelt werden.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [Eintrag über "string tolower" im Fish Shell Wiki](https://github.com/fish-shell/fish-shell/wiki/Builtin-string-tolower)