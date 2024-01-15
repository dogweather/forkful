---
title:                "Extrahieren von Teilzeichenketten"
html_title:           "Fish Shell: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Niemand hat Zeit, sich durch lange und unnötig komplizierte Code-Schnipsel zu kämpfen. Die Fish Shell bietet eine einfache und intuitive Möglichkeit, Substrings in einer Zeichenkette zu extrahieren. Das spart Zeit und Nerven, wenn man nur bestimmte Teile einer Zeichenkette benötigt.

## Wie geht’s

Führe die folgenden Schritte aus, um Substrings in der Fish Shell zu extrahieren:

1. Öffne deine Fish Shell.
2. Definiere eine Variable mit einer Zeichenkette, aus der du einen Substring extrahieren möchtest:
```Fish Shell
set text "Hallo, mein Name ist Max."
```
3. Gib den Substring-Befehl ein, gefolgt von der zu extrahierenden Länge des Substrings:
```Fish Shell
echo $text[3..9]
```
4. Drücke Enter und voila, du hast deinen Substring extrahiert! Das Ergebnis lautet: `"lo, mei"`, da `3` dem dritten Zeichen im String (`l`) und `9` dem neunten Zeichen (`i`) entspricht.

## Tief eintauchen

Die Syntax zum Extrahieren von Substrings in der Fish Shell basiert auf [Slice Notation](https://fishshell.com/docs/current/index.html#slice-notation). Dabei sind folgende Punkte zu beachten:

- Die Indexierung beginnt immer bei `1`, nicht bei `0`.
- Der letzte Index bezeichnet das Ende der Zeichenkette. In unserem Beispiel endet der Substring bei `9`, da der `9` Index das Ende des Strings `".`" markiert.
- Es können auch negative Indizes verwendet werden, um vom Ende der Zeichenkette aus zu zählen. Zum Beispiel würde `$text[-1]` den letzten Buchstaben (`"."`) ausgeben.

Falls du weitere Informationen zu Slice Notation benötigst, schaue dir die offizielle [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html#appendix-references) an.

## Siehe auch

- [Offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/#substring-expansion)
- [Slice Notation für Python](https://www.w3schools.com/python/ref_func_slice.asp)
- [Wie man Substrings in Bash  extrahiert](https://shapeshed.com/unix-extract-substring/)