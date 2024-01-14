---
title:                "Python: Ein temporäres Datei erstellen"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Temporäre Dateien sind häufig in der Programmierwelt anzutreffen und können in verschiedenen Situationen hilfreich sein. Sie werden meist dazu verwendet, um Daten temporär zu speichern, bevor sie in eine endgültige Datei geschrieben werden oder um temporäre Konfigurationsdateien zu erstellen. Der Vorteil von temporären Dateien liegt darin, dass sie automatisch vom System gelöscht werden, sobald sie nicht mehr benötigt werden. In diesem Blog-Beitrag werden wir uns genauer anschauen, wie man in Python temporäre Dateien erstellen kann und warum es nützlich ist, dies zu tun.

## Wie man temporäre Dateien in Python erstellt

Das Erstellen einer temporären Datei in Python ist dank der integrierten Bibliothek `tempfile` sehr einfach. Zunächst müssen wir die Bibliothek importieren:

```Python
import tempfile
```

Anschließend können wir mit der Funktion `NamedTemporaryFile()` eine temporäre Datei erstellen. Diese Funktion erstellt automatisch eine temporäre Datei im temporären Verzeichnis des Systems und gibt ein Dateiobjekt zurück, das wir zum Schreiben und Lesen verwenden können:

```Python
temp_file = tempfile.NamedTemporaryFile()
```

Die erstellte Datei wird automatisch im binären Lesemodus geöffnet, daher müssen wir gegebenenfalls den Modus auf den gewünschten Typ ändern. Zum Beispiel, um die Datei im Textmodus zu öffnen, können wir den Parameter `mode="w"` an die Funktion `NamedTemporaryFile()` übergeben:

```Python
temp_file = tempfile.NamedTemporaryFile(mode="w")
```

Jetzt können wir Daten in die Datei schreiben:

```Python
temp_file.write("Dies ist eine temporäre Datei.")
```

Und zum Lesen können wir entweder `read()` oder `readlines()` verwenden:

```Python
print(temp_file.read())
```

Die temporäre Datei wird automatisch gelöscht, sobald das `temp_file`-Objekt geschlossen wird. Wir können die Datei auch manuell löschen, indem wir die `delete`-Methode aufrufen:

```Python
temp_file.delete()
```

## Tiefergehende Informationen

Der Standardtupel von `tempfile.NamedTemporaryFile()` enthält sowohl die Dateiobjekt als auch den Dateinamen. Wenn wir jedoch die Namen der temporären Dateien sehen möchten, können wir den Kontextmanager `TemporaryFile` verwenden, der von der Funktion `tempfile.NamedTemporaryFile()` verwendet wird. Mit diesem Kontextmanager können wir auf den Dateinamen der erstellten temporären Datei zugreifen, ohne auf das Dateiobjekt selbst zugreifen zu müssen:

```Python
with tempfile.TemporaryFile() as temp_file:
    print(temp_file.name)
```

Es ist auch möglich, die temporäre Datei in einem bestimmten Verzeichnis zu erstellen, anstatt das standardmäßige temporäre Verzeichnis des Systems zu verwenden. Dazu können wir den Parameter `dir` an die Funktion `NamedTemporaryFile()` übergeben und den gewünschten Pfad angeben:

```Python
temp_file = tempfile.NamedTemporaryFile(dir="/home/username/temp/")
```

## Siehe auch

- [tempfile - Python-Dokumentation](https://docs.python.org/3/library/tempfile.html)
- [How to Create Temporary Files and Directories in Python](https://www.digitalocean.com/community/tutorials/how-to-create-temporary-files-and-directories-in-python)