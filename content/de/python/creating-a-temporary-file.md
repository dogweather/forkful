---
title:                "Erstellen einer temporären Datei"
html_title:           "Python: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Temporäre Dateien sind nützliche Werkzeuge für Programmierer, um Daten vorübergehend zu speichern oder um temporäre Dateien zu erstellen, die von anderen Programmen verwendet werden können. Das Erstellen von temporären Dateien lässt sich effizient und einfach mit Python umsetzen und kann in verschiedenen Anwendungsfällen hilfreich sein.

## Wie geht das?

Um eine temporäre Datei in Python zu erstellen, gibt es zwei häufig verwendete Methoden, die wir uns genauer anschauen werden.

### Methode 1: tempfile.TemporaryFile()

Die erste Methode verwendet die integrierte "tempfile" Bibliothek von Python. Sie können diese Bibliothek verwenden, um eine temporäre Datei zu erstellen, die automatisch beim Schließen gelöscht wird.

```Python
import tempfile

# Erstellt eine temporäre Datei im binären Modus (zum Lesen und Schreiben)
temp_file = tempfile.TemporaryFile()

# Schreiben von Daten in die temporäre Datei
temp_file.write(b"Dies ist ein Beispieltext.")

# Zurücksetzen des Cursors auf den Anfang der Datei
temp_file.seek(0)

# Lesen von Daten aus der temporären Datei
print(temp_file.read())

# Schließen der temporären Datei, wodurch sie automatisch gelöscht wird
temp_file.close()

# Ausgabe:
# b'Dies ist ein Beispieltext'
```

### Methode 2: NamedTemporaryFile()

Die zweite Methode verwendet ebenfalls die "tempfile" Bibliothek, ermöglicht aber die Benennung der temporären Datei. Dies kann beim Testen oder Debuggen hilfreich sein, um die erstellte Datei einfacher zu identifizieren.

```Python
import tempfile

# Erstellt eine benannte temporäre Datei im Textmodus (zum Schreiben und Lesen)
temp_file = tempfile.NamedTemporaryFile(mode="w", delete=False)

# Schreibt Daten in die temporäre Datei
temp_file.write("Dies ist ein Beispieltext.")

# Schließen der temporären Datei
temp_file.close()

# Ausgabe:
# /var/folders/y2/lc65tmy55mx8gxc47qhhdlbym0000gn/T/tmpugbpejo9
```

Beachten Sie, dass die temporäre Datei standardmäßig im binären Modus erstellt wird, daher ist es wichtig, den Modus auf "w" (Schreibmodus für Text) oder "wb" (Schreibmodus für binäre Dateien) festzulegen.

## Tiefer Einblick

Das Erstellen von temporären Dateien mit Python kann auch weiter angepasst werden, indem Sie Attribute wie "prefix", "suffix" und "dir" in der "NamedTemporaryFile()" Methode verwenden. Diese Attribute ermöglichen es Ihnen, den Dateinamen mit einem Präfix und/oder Suffix zu versehen oder den Speicherort der temporären Datei anzugeben.

```Python
# Erstellen einer temporären Datei mit Präfix und Suffix
temp_file = tempfile.NamedTemporaryFile(prefix="temp_", suffix=".txt")

# Erstellen einer temporären Datei in einem bestimmten Verzeichnis
temp_file = tempfile.NamedTemporaryFile(dir="/Users/Nutzer/Dokumente/")
```

Darüber hinaus können Sie die Option "delete=False" verwenden, um zu verhindern, dass die temporäre Datei automatisch gelöscht wird. Auf diese Weise können Sie die Datei für spätere Verwendung offen halten oder sie manuell löschen, wenn sie nicht mehr benötigt wird.

## Siehe auch

- Offizielle Dokumentation zur "tempfile" Bibliothek in Python: https://docs.python.org/3/library/tempfile.html
- Ein Tutorial über die Verwendung von temporären Dateien in Python: https://www.geeksforgeeks.org/python-temporary-file-using-tempfile/