---
title:                "Fish Shell: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Haben Sie schon einmal versucht, die Länge eines Strings in Ihrem Fish Shell Programm zu ermitteln? Vielleicht möchten Sie die Länge eines Benutzernamens oder einer E-Mail-Adresse überprüfen. In diesem Blogbeitrag zeigen wir Ihnen, wie Sie dies schnell und effizient tun können.

## So geht's

Um die Länge eines Strings in Fish Shell zu finden, können Sie das integrierte `string length` Befehl verwenden. Durch Hinzufügen des zu überprüfenden Strings als Argument erhalten Sie die Anzahl der Zeichen zurück.

```Fish Shell
string length "Hallo"
# Output: 5
```

Sie können auch eine Variable verwenden, um die Länge eines Strings zu speichern und später zu verwenden.

```Fish Shell
set str "Hello"
string length $str
set len (string length $str)
echo "Die Länge von '$str' ist $len."
# Output: Die Länge von 'Hello' ist 5.
```

Wenn Sie nur die Anzahl der Buchstaben in einem String zählen möchten, können Sie die Option `-b` verwenden.

```Fish Shell
string length -b "Hello World"
# Output: 11
```

## Tiefer tauchen

Nun, wo Sie wissen, wie Sie die Länge eines Strings finden, sollten Sie sich bewusst sein, dass diese Methode je nach Umgebung unterschiedlich sein kann. Wenn Sie beispielsweise einen Unicode-String in einem Terminal mit einer begrenzten Zeichenfolge ausgeben, wird die Länge möglicherweise nicht korrekt angezeigt.

Um diese Probleme zu vermeiden, gibt es in Fish Shell die Funktion `count` im Modul `string`. Diese berücksichtigt die tatsächliche Länge eines Unicode-Strings und gibt das korrekte Ergebnis zurück.

## Siehe auch

- [`string length` Dokumentation](https://fishshell.com/docs/current/cmds/string-length.html)
- [`count` Modul Dokumentation](https://fishshell.com/docs/current/cmds/count.html)