---
title:                "Großschreibung eines Strings"
html_title:           "Fish Shell: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum?

Es gibt viele Gründe, warum man eine Zeichenkette in Großbuchstaben umwandeln möchte. Vielleicht möchtest du sicherstellen, dass Nutzereingaben korrekt formatiert werden, oder du möchtest eine einheitliche Darstellung von Texten in deiner Anwendung erreichen. In jedem Fall ist ein grundlegendes Verständnis der Groß- und Kleinschreibung in der Programmierung wichtig und das Umgang mit Strings ist eine grundlegende Fähigkeit, die du beherrschen solltest.

## Wie geht das?

Die gute Nachricht ist, dass es mit der Fish-Shell ganz einfach ist, eine Zeichenkette in Großbuchstaben zu konvertieren. Hier ist ein Beispiel:

```
set my_string "hallo welt"
echo $my_string | tr "[:lower:]" "[:upper:]"
```

Der Befehl `set` definiert eine Variable mit dem Wert "hallo welt". Dann wird der Befehl `echo` verwendet, um den grünen Teil der Pipeline auszuführen und den Wert der Variable auszugeben. Die Pipeline wird an den nächsten Befehl `tr` übergeben, der alle Kleinbuchstaben in dem übergebenen Text in Großbuchstaben umwandelt. In diesem Fall wird also "HALLO WELT" ausgegeben.

Du kannst auch eine ganze Datei in Großbuchstaben umwandeln, indem du den `tr`-Befehl mit dem Input von `cat` kombinierst. Hier ist ein Beispiel, das den Text in einer Datei namens "meintext.txt" in Großbuchstaben ausgibt:

```
cat meintext.txt | tr "[:lower:]" "[:upper:]"
```

## Tief eintauchen

Um ein tieferes Verständnis dafür zu entwickeln, wie die Groß- und Kleinschreibung in der Programmierung funktioniert, ist es hilfreich, sich mit der Unicode-Charakterkodierung zu beschäftigen. Kurz gesagt, Unicode ermöglicht es, alle Zeichen und Symbole in einer einzigen Kodierung zu verwenden, anstatt verschiedene Kodierungen für verschiedene Sprachen zu haben.

In der Unicode-Kodierung gibt es zwei Arten von Zeichen: Großbuchstaben und Kleinbuchstaben. Die Umschaltung von Groß- zu Kleinbuchstaben erfolgt durch Hinzufügen oder Entfernen eines bestimmten Werts zu dem numerischen Wert des Zeichens. Zum Beispiel hat das kleine "a" einen numerischen Wert von 97, während das große "A" einen Wert von 65 hat. Indem du den Wert 32 zu dem Buchstaben $a$ addierst, erhältst du den Wert 97 + 32 = 129, der dem großen "A" entspricht.

Im Beispiel mit der `tr`-Pipeline fügen wir dieselbe Menge (32) dem numerischen Wert jedes Kleinbuchstabens hinzu, um ihn in einen Großbuchstaben zu konvertieren.

In diesem Sinne kannst du mit der Fish-Shell noch mehr manipulieren, indem du andere Werte als 32 verwendest. Zum Beispiel, wenn du deinen Text in kleine Buchstaben umwandeln möchtest, musst du 32 von dem numerischen Wert jedes Großbuchstabens abziehen.

## Siehe auch

- [Unicode-Kodierungen](https://de.wikipedia.org/wiki/Unicode): Erfahre mehr über die zugrundeliegende Technologie für die Darstellung von Zeichen.
- [Fish-Shell-Dokumentation](https://fishshell.com/docs/current/): Finde weitere hilfreiche Informationen über die Verwendung der Fish-Shell.
- [Tr-Manual](https://www.gnu.org/software/coreutils/tr): Lies die offizielle Dokumentation für den `tr`-Befehl.