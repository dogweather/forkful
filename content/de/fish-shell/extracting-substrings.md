---
title:                "Fish Shell: Untersuchen von Teilzeichenketten"
simple_title:         "Untersuchen von Teilzeichenketten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Du fragst dich vielleicht, warum es wichtig ist, Substrings zu extrahieren, wenn du Fish Shell benutzt. Die einfache Antwort ist, dass es dir helfen kann, bestimmte Teile von einem längeren Text zu isolieren oder zu entfernen. Das kann besonders hilfreich sein, wenn du mit großen Datenmengen arbeitest oder wenn du bestimmte Zeichenfolgen aus einer Ausgabe entfernen musst.

## Wie geht das?

Um Substrings in Fish Shell zu extrahieren, kannst du den Befehl `string match` verwenden. Du musst dabei zwei Argumente angeben: die Zeichenfolge, aus der du den Substring extrahieren möchtest, und das Muster, das du verwenden willst, um den Substring zu identifizieren.

```Fish Shell
set string "Hallo Welt"
string match Welt $string
```

Das obige Beispiel wird den Substring "Welt" aus der Zeichenfolge "Hallo Welt" extrahieren und diese Ausgabe zurückgeben:

```Fish Shell
Welt
```

Du kannst auch Platzhalter verwenden, um mehrere Zeichenfolgen mit ähnlichen Mustern zu identifizieren. Zum Beispiel können die Platzhalter `?` und `*` verwendet werden, um einzelne bzw. beliebig viele Zeichen zu repräsentieren.

```Fish Shell
string match Ha?e $string
```

Dieses Beispiel wird den Substring "Hall" aus der Zeichenfolge "Hallo Welt" extrahieren.

## Tiefergehende Infos

Abgesehen von einfachen Platzhaltern, kannst du mit dem Befehl `string match` auch reguläre Ausdrücke verwenden, um Substrings zu extrahieren. Reguläre Ausdrücke bieten eine flexiblere und leistungsstärkere Möglichkeit, Muster zu definieren und Substrings zu identifizieren.

Mit `string match -r` kannst du reguläre Ausdrücke in deinem Suchmuster verwenden. Zum Beispiel kannst du damit alle Wörter in einer Zeichenfolge extrahieren, die mit dem Buchstaben "a" beginnen.

```Fish Shell
set string "Alles anfangen macht Spaß"
string match -r a\w+ $string
```

Dieses Beispiel wird folgendes ausgeben:

```Fish Shell
Alles
anfangen
aus
Spaß
```

## Siehe auch

- [Fish Shell Dokumentation zu string match](https://fishshell.com/docs/current/cmds/string-match.html)
- [Reguläre Ausdrücke in Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-tut7)
- [Einführung zu regulären Ausdrücken](https://www.regular-expressions.info/tutorial.html)