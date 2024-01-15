---
title:                "Die Länge einer Zeichenkette finden"
html_title:           "Fish Shell: Die Länge einer Zeichenkette finden"
simple_title:         "Die Länge einer Zeichenkette finden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt mit der Länge eines Strings beschäftigen? Nun, in der Programmierung ist es manchmal wichtig zu wissen, wie viele Zeichen ein String enthält, um zum Beispiel die Ausgabe eines Textes anzupassen.

## Wie geht's
#### Fish Shell Installation
Bevor wir loslegen können, muss die Fish Shell installiert werden. Dies kann einfach über einen Paketmanager wie Homebrew oder Aptitude geschehen.

#### Länge eines Strings finden
Um die Länge eines Strings zu finden, können wir die `string` Funktion `length` verwenden. Hier ein Beispiel:
```Fish Shell
set name "John"
echo (string length $name)
```
Dieser Code gibt die Länge des Strings "John" aus, in diesem Fall also die Zahl 4.

## Tiefergehende Information
Wie funktioniert die `length` Funktion eigentlich? Sie zählt einfach die Anzahl an Zeichen im String und gibt diese als Zahl zurück. Beachte, dass Leerzeichen und Sonderzeichen auch als Zeichen gezählt werden.

## Siehe Auch
- [Offizielle Dokumentation der `length` Funktion](https://fishshell.com/docs/current/cmds/string.html#length)
- [String Manipulation mit Fish Shell](https://dev.to/mishmanners/working-with-strings-in-fish-shell-4he7)