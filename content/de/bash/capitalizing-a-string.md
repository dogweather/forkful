---
title:                "Bash: Ein String großschreiben"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum
Das Kapitalisieren von Strings ist ein wichtiger Teil der Programmierung, da es uns ermöglicht, Texte in Großbuchstaben darzustellen. Dies kann in verschiedenen Szenarien nützlich sein, wie z.B. bei der Validierung von Benutzereingaben oder der Erstellung von Benennungen in einer Anwendung.

## Wie geht man vor
Um einen String in Bash zu kapitalisieren, gibt es mehrere Ansätze. Wir werden hier die folgenden beiden Methoden betrachten:
1. Verwendung der internen Shell Variablen `^^`
2. Verwendung der `tr` Funktion

### Methode 1 - Verwendung von `^^`
Diese Methode ist relativ neu und wurde in Bash 4.0 eingeführt. Sie verwendet die interne Shell-Variable `^^`, um einen String in Großbuchstaben umzuwandeln. Hier ist ein Beispielcode, der eine interaktive Eingabe vom Benutzer entgegennimmt und sie dann in Großbuchstaben ausgibt:

```Bash
echo "Bitte gib deinen Namen ein:"
read name
echo "Hallo ${name^^}!"
```

Die Ausgabe für die Eingabe von "Peter" wäre:

```Bash
Hallo PETER!
```

Wie man sieht, wird der String `^^` für die Kapitalisierung verwendet.

### Methode 2 - Verwendung von `tr`
Die `tr`-Funktion ist ein älterer Ansatz zur String-Manipulation und wird in vielen Unix-basierten Systemen verwendet. Sie kann verwendet werden, um einzelne Zeichen oder Zeichenfolgen in einem String zu ersetzen. Hier ist ein Beispielcode, der dieselbe Funktionalität wie die vorherige Methode erreicht:

```Bash
echo "Bitte gib deinen Namen ein:"
read name
echo "Hallo $(echo $name | tr '[:lower:]' '[:upper:]')!"
```

Die Ausgabe würde wiederum "Hallo PETER!" sein.

### Bemerkungen zur Performance
Bei der Verwendung der `tr`-Funktion kann es zu einer geringeren Performance im Vergleich zur Variante mit `^^` kommen, da sie ein externes Programm aufruft. Auch die Lesbarkeit des Codes kann durch die Verwendung von `tr` beeinträchtigt werden.

## Tiefergehende Informationen
Beim Kapitalisieren von Strings gibt es auch bestimmte Ausnahmen zu beachten, wie z.B. die Behandlung von Sonderzeichen oder Umlauten. Diese müssen eventuell gesondert behandelt werden, um unerwünschte Ergebnisse zu vermeiden. Eine erweiterte Diskussion darüber würde jedoch den Rahmen dieses Artikels sprengen.

## Siehe auch
- [Bash Referenz](https://www.gnu.org/software/bash/manual/bashref.html)
- [Detaillierte Erklärung zu `tr`](https://linux.die.net/man/1/tr)