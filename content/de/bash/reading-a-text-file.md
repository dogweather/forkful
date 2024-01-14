---
title:    "Bash: Das Lesen einer Textdatei"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist ein grundlegender Bestandteil der Bash-Programmierung und kann Ihnen helfen, wertvolle Informationen aus Ihren Dateien zu extrahieren. Wenn Sie lernen, wie Sie Textdateien in Bash lesen, können Sie Ihre Skripte noch leistungsstärker gestalten und eine Vielzahl von Aufgaben automatisieren.

## Wie man Textdateien in Bash liest

Das Lesen von Textdateien in Bash ist recht einfach und erfordert nur wenige Zeilen Code. Zunächst müssen Sie die Datei öffnen und dann mithilfe einer Schleife durch jede Zeile in der Datei iterieren. Innerhalb dieser Schleife können Sie dann die gewünschten Aktionen ausführen. Hier ist ein Beispielcode:

```Bash
#!/bin/bash

# Datei öffnen
file="beispiel.txt"

# Schleife durch jede Zeile iterieren
while read line
do
# Hier können Sie Aktionen mit jeder Zeile ausführen, z.B. Ausgabe auf der Konsole
echo $line
done < $file
```

Das obige Beispiel liest die Datei "beispiel.txt" Zeile für Zeile und gibt jede Zeile auf der Konsole aus. Sie können diesen Code anpassen, je nachdem, welche Art von Aktionen Sie mit den Zeilen ausführen möchten.

## Tiefergehende Informationen

Es gibt auch Möglichkeiten, die Standardtrennzeichen oder das Trennzeichen zwischen den einzelnen Spalten in einer Textdatei zu ändern. Dies kann hilfreich sein, wenn Sie mit komplexeren Dateien arbeiten. Sie können auch spezifische Zeilen basierend auf bestimmten Kriterien aus der Datei auswählen. All diese tiefergehenden Informationen helfen Ihnen dabei, Ihre Bash-Programmierung noch effektiver zu gestalten.

## Siehe auch

- [Bash-Programmierung für Anfänger](https://www.linux.com/training-tutorials/learn-bash-scripting-basics-beginners/)
- [Offizielle Dokumentation für die Bash-Skriptsprache](https://www.gnu.org/software/bash/)
- [10 praktische Beispiele für die Bash-Programmierung](https://www.tecmint.com/10-practical-examples-of-linux-bash-scripting/)