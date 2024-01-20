---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Der Prozess des Wandels einer Zeichenkette (oder String) in Kleinschreibung ist ein gebräuchliches Muster in Computerprogrammierung. Dies kann hilfreich sein, wenn wir eine standardisierte Verarbeitung oder einen Vergleich von Textdaten durchführen möchten, unabhängig von der Eingabeformatierung.

## So geht's:

Zum Umwandeln von Strings in Kleinschreibung in der Fish Shell verwenden wir die `string lower` Funktion. Hier ist ein einfaches Beispiel:

```Fish Shell
set meinText "Dies Ist Mein TEXT"
set lowerCaseText (string lower -a $meinText)
echo $lowerCaseText
```

Die Output wird wie folgt sein:

```Fish Shell
dies ist mein text
```
Die Option `-a` gewährleistet, dass jeder Buchstabe des Strings in Kleinschreibung konvertiert wird.

## Tiefer Einblick:

Die Möglichkeit, Strings in der Fish Shell in Kleinbuchstaben umzuwandeln, ist ein relativ neues Merkmal, das in der Version 3.1 eingeführt wurde. Es gibt zahlreiche Alternativen in anderen Shell-Skriptsprachen, darunter `tr` in Bash und die Methode `downcase` in Ruby.

Die Implementierungsdetails von `string lower` in Fish sind recht einfach. Die Funktion durchläuft jedes Zeichen im String und verwendet eine einfache Funktion zur Konvertierung von Groß- zu Kleinbuchstaben, die auf der ASCII-Tabelle basiert. Es wird vorausgesetzt, dass die Eingabe ein gültiger UTF-8-Text ist.

## Siehe auch:

Für weitere Informationen zu Funktionen und Befehlen in Fish Shell:

1. [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
2. [Fish Shell GitHub Repository](https://github.com/fish-shell/fish-shell)

Für Details zur Textverarbeitung und zur Wandlung von Zeichenketten in Kleinschreibung:

1. [Stackoverflow: How to convert a string to lower case in Bash?](https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash)
2. [Ruby Doc: Downcase method](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)