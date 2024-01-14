---
title:                "Bash: Einen String in Kleinbuchstaben umwandeln"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings in Kleinbuchstaben ist ein häufiger Schritt bei der Bearbeitung von Texten in der Bash-Programmierung. Es kann hilfreich sein, um den Vergleich von Strings zu erleichtern oder einheitliche Ausgabeformate zu erzielen.

## Anleitung

Hier zeigen wir, wie man in Bash eine Zeichenkette in Kleinbuchstaben umwandelt:

```
# Beispieldaten
text="HALLO WELT"

# Konvertieren in Kleinbuchstaben
echo "${text,,}"

# Ausgabe: hallo welt
```

Das Komma in der Variable zeigt an, dass der Inhalt in Kleinbuchstaben umgewandelt werden soll. Diese Methode funktioniert auch mit Variablen, die mehrere Wörter enthalten:

```
# Beispieldaten
name="MAX MUSTERMANN"

# Konvertieren in Kleinbuchstaben
echo "${name,,}"

# Ausgabe: max mustermann
```

Man kann auch nur einen Teil der Zeichenkette konvertieren, indem man einen Bereich angeben:

```
echo "${text,,4}"

# Ausgabe: HALLO welt
```

Das bedeutet, dass nur die Teile der Zeichenkette ab dem vierten Buchstaben in Kleinbuchstaben umgewandelt werden.

## Tiefere Einblicke

Die Bash bietet auch weitere Möglichkeiten, um Zeichenketten in Kleinbuchstaben zu konvertieren. Hier sind einige Beispiele:

- Um alle Buchstaben in einer Zeichenkette groß zu schreiben, benutzt man `^^` statt `,,`.
- Wenn man nur den ersten Buchstaben in Kleinbuchstaben schreiben möchte, kann man `,,1` benutzen.
- Es gibt auch eine Option `,,*` um alle Zeichen der Zeichenkette in Kleinbuchstaben umzuwandeln.

Um mehr über diese Methoden und weitere zu erfahren, kann man sich die offizielle Bash-Dokumentation ansehen.

## Siehe auch

- [Offizielle Bash-Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash-Programmierung für Anfänger](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html)
- [Einführung in die Bash-Shell](https://www.heise.de/ct/artikel/Grundlagen-der-Bash-Shell-2056759.html)