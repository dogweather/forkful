---
title:                "Eine Textdatei schreiben"
html_title:           "Python: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit, die jeder Python-Programmierer beherrschen sollte. Textdateien sind ein einfaches und effizientes Format zum Speichern und Lesen von Daten. Sie sind auch für die Zusammenarbeit mit anderen Programmierern und das Speichern von Ergebnissen nützlich.

## Wie geht's

Das Erstellen und Schreiben von Textdateien in Python ist sehr einfach. Folgendes Beispiel zeigt, wie man eine neue Textdatei mit dem Namen "beispiel.txt" erstellt und Text in die Datei schreibt:

```Python
with open("beispiel.txt", "w") as f:
    f.write("Dies ist ein Beispieltext")
```

Durch das Verwenden des "with" -Befehls wird die Datei automatisch geschlossen, sobald der Block beendet ist. Die "w" -Option im "open" -Befehl gibt an, dass die Datei zum Schreiben geöffnet wird.

Um Text in eine vorhandene Datei zu schreiben, öffnen Sie sie einfach mit der "a" -Option und verwenden Sie "f.write" wie im folgenden Beispiel:

```Python
with open("beispiel.txt", "a") as f:
    f.write("Dies ist ein neuer Text, der zur Datei hinzugefügt wurde")
```

## Tiefere Einblicke

Neben dem Schreiben von Text können Sie in Python auch verschiedene andere Datentypen, wie z.B. Listen oder Dictionaries, in eine Textdatei schreiben. Um dies zu tun, müssen Sie die Daten zuerst in ein String-Format umwandeln. Hier ist ein Beispiel, das eine Liste in eine Textdatei schreibt:

```Python
beispiel_liste = ["Hund", "Katze", "Vogel"]

with open("tiere.txt", "w") as f:
    f.write(",".join(beispiel_liste))
```

Die "join" -Methode wird verwendet, um die Elemente der Liste zu einem String zu verbinden, der dann in die Datei geschrieben wird. Die Elemente werden dabei durch das angegebene Trennzeichen (hier ein Komma) getrennt.

## Siehe auch

- [Offizielle Python-Dokumentation zur Arbeit mit Dateien](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial zur Verwendung von "with" in Python](https://realpython.com/python-with-statement/)
- [Weitere Informationen zum Schreiben von Dateien in Python](https://www.geeksforgeeks.org/reading-writing-text-files-python/)