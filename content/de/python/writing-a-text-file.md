---
title:    "Python: Eine Textdatei schreiben"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

Bevor wir uns damit beschäftigen, wie man eine Textdatei in Python programmieren kann, wollen wir uns kurz die Frage stellen, warum wir überhaupt eine Textdatei erstellen sollten. Im Alltag nutzen wir häufig Textdateien, um Informationen zu speichern, die wir später wieder abrufen können. Eine Textdatei ist dabei nichts anderes als ein Dokument, das nur aus reinem Text besteht, ohne Formatierungen oder spezielle Funktionen. In der Programmierung können wir Textdateien nutzen, um Daten zu speichern oder Protokolle zu erstellen. Sie sind also ein wichtiger Bestandteil unserer Programmierarbeit.

## Wie man eine Textdatei in Python erstellt

Um eine Textdatei in Python zu erstellen, nutzen wir die Funktion `open()`. In der Klammer hinter der Funktion geben wir den Namen der Datei an, die wir erstellen wollen, sowie den Modus, in dem die Datei geöffnet werden soll. Hier ein Beispiel:

```Python
f = open("meine_datei.txt", "w")
```

Der Modus `w` bedeutet, dass wir die Datei zum Schreiben öffnen. Danach können wir mit der Funktion `write()` Text in die Datei schreiben. Im folgenden Beispiel fügen wir drei Zeilen Text hinzu:

```Python
f.write("Hallo, dies ist meine erste Textdatei.\n")
f.write("Ich habe sie mit Python erstellt.\n")
f.write("Programmieren macht Spaß!")
```

Die Funktion `write()` fügt den angegebenen Text immer am Ende der Datei hinzu. Die `\n`-Zeichen sorgen für einen Zeilenumbruch. Am Ende müssen wir die Datei mit der Funktion `close()` schließen, damit sie gespeichert wird:

```Python
f.close()
```

Wenn wir unsere Textdatei nun öffnen, sollten die drei Zeilen darin erscheinen.

## Tiefergehende Informationen zu Textdateien

Um eine Textdatei in Python zu öffnen, können wir auch den Modus `r` für Lesen verwenden. Mit der Funktion `read()` können wir dann den gesamten Inhalt der Datei in einen String speichern und ihn zum Beispiel auf der Konsole ausgeben.

Eine weitere nützliche Funktion beim Umgang mit Textdateien ist `readline()`. Diese liest immer eine Zeile der Datei und setzt danach den Cursor auf die nächste Zeile. So können wir gezielt bestimmte Informationen aus der Datei auslesen.

Es ist außerdem wichtig zu beachten, dass jede Zeile in einer Textdatei mit dem Zeichen `\n` endet, außer die letzte Zeile. Dies kann beim Umgang mit Textdateien manchmal zu unerwarteten Ergebnissen führen.

## Siehe auch

Für weitere Informationen zum Umgang mit Textdateien in Python empfehle ich folgende Links:

- Offizielle Python-Dokumentation zu Datei-Operationen: https://docs.python.org/de/3/tutorial/inputoutput.html#reading-and-writing-files
- Ein Tutorial zur Arbeit mit Textdateien in Python: https://realpython.com/read-write-files-python/
- Tutorial zur Verwendung der `open()`-Funktion: https://www.tutorialspoint.com/python/file_open.htm