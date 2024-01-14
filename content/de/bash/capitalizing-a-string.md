---
title:                "Bash: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Zeichenketten ist eine häufig verwendete Funktion in der Bash-Programmierung. Es ermöglicht es uns, Texte auf eine bestimmte Art und Weise zu formatieren und kann in verschiedenen Anwendungsfällen nützlich sein.

## Wie es geht

Um eine Zeichenkette in Bash zu kapitalisieren, können wir das integrierte Tool `tr` verwenden. Hier ist ein Beispielcode für eine Funktion, die einen String in Großbuchstaben umwandelt:

```Bash
# Funktion zum Kapitalisieren von Strings
capitalize() {
  # Speichern des Strings in einer Variable
  string=$1

  # Verwendung von tr, um die Zeichenkette in Großbuchstaben umzuwandeln
  capitalized_string="$(echo "${string}" | tr '[:lower:]' '[:upper:]')"

  # Ausgabe des Ergebnisses
  echo "${capitalized_string}"
}

# Aufruf der Funktion mit einem Beispiel-String
capitalize "hallo welt"

# Ausgabe: HALLO WELT
```

Wir speichern zuerst den übergebenen String in einer Variablen und verwenden dann die `tr`-Funktion, um alle kleinen Buchstaben in Großbuchstaben umzuwandeln. Das Ergebnis wird in einer neuen Variable gespeichert und schließlich ausgegeben.

## Deep Dive

Wenn wir uns das Beispiel oben genauer ansehen, können wir sehen, dass wir die `tr`-Funktion verwenden, um Zeichen in unserem String zu ersetzen. Wir geben zwei Zeichensätze an, einen mit allen Kleinbuchstaben (`[:lower:]`) und einen mit allen Großbuchstaben (`[:upper:]`). Das bedeutet, dass alle Vorkommen von Kleinbuchstaben in unserem String durch die entsprechenden Großbuchstaben ersetzt werden.

Es gibt auch andere Möglichkeiten, einen String in Bash zu kapitalisieren, zum Beispiel mit dem `sed` Befehl. Es gibt jedoch keine integrierte Funktion oder Befehl, der direkt dazu dient, einen String in Bash zu kapitalisieren.

## Siehe auch

- [Offizielle Dokumentation zu tr](https://linux.die.net/man/1/tr)
- [Stack Overflow-Antwort zum Kapitalisieren von Strings in Bash](https://stackoverflow.com/a/2264537)
- [Video-Tutorial zu tr in der Bash-Programmierung](https://www.youtube.com/watch?v=rJtViH_uQu8)