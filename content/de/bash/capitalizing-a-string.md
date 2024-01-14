---
title:    "Bash: Ein String großschreiben"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Warum

Ein häufiges Problem beim Programmieren in Bash ist es, Strings oder Zeichenketten korrekt zu formatieren. Oftmals müssen Wörter oder ganze Sätze großgeschrieben werden, sei es für die Darstellung in einer Benutzeroberfläche oder für die Weiterverarbeitung in einem Skript. In diesem Blogpost wollen wir uns die Möglichkeit anschauen, in Bash Strings zu großschreiben.

## Wie geht das?

Um einen String in Bash zu großzuschreiben, gibt es verschiedene Möglichkeiten. Eine einfache Methode ist die Verwendung der internen `tr` Funktion:

```bash
echo "hallo welt" | tr '[:lower:]' '[:upper:]'
```

Dieses Beispiel wird den String "hallo welt" in "HALLO WELT" umwandeln. Der `tr` Befehl verwendet reguläre Ausdrücke, um bestimmte Zeichen oder Zeichengruppen zu ersetzen.

Eine andere Möglichkeit ist die Verwendung der `awk` Funktion, die es ermöglicht, das erste Zeichen jedes Wortes in Großbuchstaben zu setzen:

```bash
echo "hallo welt" | awk '{for(i=1;i<=NF;i++)$i=toupper(substr($i,1,1))substr($i,2)}1'
```

Dieses Beispiel wird die Ausgabe "Hallo Welt" erzeugen. Beachte, dass dies nur für die ersten Buchstaben jedes Wortes gilt, während alle anderen Buchstaben kleingeschrieben werden.

## Tiefer Einblick

Um zu verstehen, wie diese Methoden funktionieren, ist ein tieferer Einblick in die `tr` und `awk` Funktionen hilfreich.

`tr` steht für "translate" und kann verwendet werden, um das Auftreten eines oder mehrerer Zeichen durch ein anderes Zeichen zu ersetzen. In unserem Beispiel verwenden wir reguläre Ausdrücke, um alle Kleinbuchstaben in Großbuchstaben zu übersetzen. Die Option `[:lower:]` gibt dabei alle Kleinbuchstaben an, während `[:upper:]` alle Großbuchstaben angibt, die als Ersatz verwendet werden.

`awk` ist eine mächtige textbasierte Skriptsprache, die hauptsächlich für die Verarbeitung von Daten und Textdateien verwendet wird. Mit `awk` können wir die `toupper` Funktion verwenden, um das erste Zeichen in Großbuchstaben zu setzen, während `substr` verwendet wird, um den restlichen Teil des Wortes unverändert zu lassen.

## Siehe auch

- [Bash Reference Manual - tr](https://www.gnu.org/software/bash/manual/html_node/The-tr-Builtin.html)
- [GNU Awk User’s Guide - toupper](https://www.gnu.org/software/gawk/manual/html_node/String-Functions.html#String-Functions)