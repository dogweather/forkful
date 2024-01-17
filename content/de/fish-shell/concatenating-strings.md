---
title:                "Verkettung von Zeichenketten"
html_title:           "Fish Shell: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Programmieren geht es oft darum, Texte zu manipulieren und zu kombinieren, um die gewünschte Ausgabe zu erzielen. Eines der Werkzeuge, die wir dafür verwenden können, ist das Verketten von Zeichenketten. Dadurch können wir mehrere Textabschnitte zu einem einzigen String zusammenfügen.

## Wie geht's:

```Fish Shell``` hat die Funktion ```string concat``` eingebaut, die uns ermöglicht, Strings zu verketten. Wir geben einfach die zu verbindenden Strings als Argumente ein und die Funktion gibt uns den kombinierten String zurück.

Beispielcode:

```
string concat "Hallo" "Welt"
```

Ausgabe:

```
HalloWelt
```

Wir können auch Variablen mit ```string concat``` kombinieren:

```
set var1 "Hallo"
set var2 "Welt"
string concat $var1 $var2
```

Ausgabe:

```
HalloWelt
```

## Tiefentauchen:

Das Konkatenieren von Strings wird bereits seit den Anfängen der Programmierung verwendet und ist eine der grundlegenden Operationen. Es gibt auch andere Möglichkeiten, Strings zu verbinden, wie zum Beispiel die Verwendung des Operators ```+``` oder der Funktion ```strcat```.

Die Implementierung von ```string concat``` in der Fish Shell basiert auf der C-Funktion ```strcat```, die zwei Strings miteinander verbindet.

## Siehe auch:

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/cmds/string.html)
- [Alternative Methoden zum Verketten von Strings](https://www.quora.com/What-are-the-different-ways-of-concatenating-strings-in-Python)