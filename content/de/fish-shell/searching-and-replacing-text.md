---
title:                "Suchen und Ersetzen von Text"
html_title:           "Fish Shell: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Manchmal möchtest du vielleicht bestimmte Textabschnitte in deinen Dateien ersetzen oder bearbeiten. Vielleicht hast du einen Codeblock, den du in mehreren Dateien duplizieren möchtest, aber jedes Mal mühsam die gleichen Änderungen vornehmen musst. Oder du möchtest einfach nur Tippfehler in deinen Dateien korrigieren. In solchen Fällen kann die Suche und Ersetzungsfunktion in der Fish Shell sehr praktisch sein, um effizient und schnell Text zu bearbeiten.

## Wie geht's?

Um Text in der Fish Shell zu suchen und zu ersetzen, kannst du das `sed`-Befehlszeilen-Tool verwenden. Dieses Tool ermöglicht es dir, eine bestimmte Zeichenfolge in einer Datei zu suchen und durch eine andere zu ersetzen. Hier ist ein Beispiel, wie du alle Vorkommen von "Hallo" in einer Datei durch "Hi" ersetzen kannst:

```
Fish Shell sed -i 's/Hallo/Hi/g' file.txt
```

In diesem Beispiel wird der Befehl `sed` verwendet, gefolgt von der Option `-i`, die dafür sorgt, dass die Änderungen direkt in der Datei gespeichert werden. Das `s` steht für "suchen", gefolgt von der zu suchenden Zeichenfolge "Hallo" und der zu ersetzenden Zeichenfolge "Hi". Das `g` bedeutet, dass alle Vorkommen innerhalb der Datei ersetzt werden sollen. Zum Schluss gibst du einfach den Dateinamen an, in dem die Änderungen durchgeführt werden sollen.

Du kannst auch mithilfe von regulären Ausdrücken gezielt nach bestimmten Mustern suchen und ersetzen. Hier ist ein Beispiel, wie du alle Dateien in einem Verzeichnis durch eine neue Dateierweiterung ".new" ersetzen kannst:

```
Fish Shell sed -i 's/\..*/.new/g' *
```

Das `.*` steht hier für "alle Zeichen nach einem Punkt", sodass alle Vorkommen von Dateierweiterungen durch ".new" ersetzt werden. Der `*` bedeutet, dass alle Dateien im aktuellen Verzeichnis bearbeitet werden sollen.

## Tiefer eintauchen

Der `sed`-Befehl bietet noch viele weitere Möglichkeiten und Optionen für die Suche und Ersetzung von Text. Du kannst zum Beispiel auch nur bestimmte Zeilen in einer Datei bearbeiten oder die Suche und Ersetzung auf bestimmte Zeichenbereiche beschränken. Um mehr über die verschiedenen Optionen von `sed` zu erfahren, kannst du die offizielle Dokumentation lesen.

## Siehe auch

* Die `sed`-Dokumentation: https://www.gnu.org/software/sed/manual/sed.html 
* Ein interaktiver Lernkurs zu `sed`: https://www.openvim.com/tutorial sed