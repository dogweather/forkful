---
title:                "Python: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Es gibt verschiedene Gründe, warum man in der Programmierung temporäre Dateien erstellen möchte. Ein häufiger Grund ist das temporäre Speichern von Daten, die später nicht mehr benötigt werden. Dies kann beispielsweise bei der Verarbeitung großer Datenmengen oder beim Testen von Code nützlich sein.

## Wie man temporäre Dateien in Python erstellt
Um eine temporäre Datei in Python zu erstellen, können wir das `tempfile` Modul verwenden. Zuerst müssen wir das Modul importieren:

```python
import tempfile
```

Dann erstellen wir eine temporäre Datei mithilfe der Funktion `tempfile.NamedTemporaryFile()`, die standardmäßig im temporären Verzeichnis des Betriebssystems erstellt wird:

```python
temp_file = tempfile.NamedTemporaryFile()
```

Wir können auch einen bestimmten Dateinamen und Dateierweiterung angeben:

```python
temp_file = tempfile.NamedTemporaryFile(suffix=".txt", prefix="temp_", dir="/home/user/temp")
```

Durch das Argument `delete=False` verhindern wir, dass die Datei automatisch gelöscht wird, sobald sie geschlossen wird. Dies kann hilfreich sein, wenn wir die Datei später noch einmal öffnen wollen.

Die erstellte temporäre Datei kann dann wie eine reguläre Datei verwendet werden, z.B. können wir Text zu der Datei schreiben:

```python
temp_file.write("Dies ist eine temporäre Datei.")
```

Und sie lesen:

```python
temp_file.read()
```

Wenn wir fertig sind, sollten wir die Datei wieder schließen und löschen:

```python
temp_file.close()
```

## Tieferer Einblick
Das `tempfile` Modul bietet auch weitere Funktionen, um temporäre Verzeichnisse oder Dateien mit benutzerdefinierten Eigenschaften zu erstellen. Es ist auch möglich, temporäre Dateien zu erstellen, die im Speicher anstatt auf der Festplatte gespeichert werden. Wenn wir mehr Kontrolle über die Erstellung von temporären Dateien benötigen, lohnt es sich, sich genauer mit dem Modul auseinanderzusetzen.

## Siehe auch
- [Dokumentation des tempfile Moduls](https://docs.python.org/de/3/library/tempfile.html)
- [Tutorial zur Nutzung von temporären Dateien in Python](https://realpython.com/python-tempfile/)
- [YouTube Video über die Verwendung von `tempfile` in Python](https://www.youtube.com/watch?v=avVc-RgKSIY)