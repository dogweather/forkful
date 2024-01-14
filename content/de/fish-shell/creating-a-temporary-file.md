---
title:                "Fish Shell: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist oft eine nützliche Technik beim Programmieren. Zum Beispiel können temporäre Dateien verwendet werden, um Zwischenergebnisse zu speichern oder um Dateien für einen bestimmten Zweck zu erstellen, ohne den ursprünglichen Code zu ändern.

## Wie man eine temporäre Datei erstellt

Das Erstellen einer temporären Datei ist mit Fish Shell sehr einfach. Verwende einfach den Befehl `mktemp` gefolgt von einem gewünschten Dateinamen oder Muster. Hier ist ein Beispiel:

```Fish Shell
mktemp temp_file_XXX.txt
```

Dieser Befehl wird eine temporäre Datei mit dem Namen "temp_file_XXX.txt" erstellen, wobei die XXX durch eine zufällige Zeichenfolge ersetzt werden. Du kannst auch ein bestimmtes Muster angeben, indem du das Zeichen "X" mit anderen Zeichen ersetzt. Zum Beispiel:

```Fish Shell
mktemp temp_file_XXXX.txt
```

Diese Methode wird eine temporäre Datei mit vier zufälligen Zeichen zwischen "temp_file_" und ".txt" erstellen.

## Tiefer eintauchen

Das Erstellen einer temporären Datei mit Fish Shell ermöglicht es dir auch, verschiedene Optionen zu definieren, wie zum Beispiel das Erstellen einer temporären Verzeichnisstruktur oder das Ändern des Präfixes der zufälligen Zeichenfolge. Weitere Informationen findest du in der Fish Shell Dokumentation.

## Siehe auch

- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- Einführung in die Programmierung mit Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Praktische Tipps und Tricks für Fish Shell: https://fishshell.com/docs/current/index.html#tips-and-tricks