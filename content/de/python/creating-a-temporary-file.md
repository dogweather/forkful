---
title:    "Python: Erstellen einer temporären Datei"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist eine häufige Aufgabe in der Programmierung, die Ihnen dabei hilft, Daten temporär zu speichern und zu verarbeiten, ohne dass es zu Konflikten mit bereits bestehenden Dateien kommt. Es ist besonders nützlich, wenn Sie mehrere Prozesse parallel ausführen oder temporäre Daten schnell löschen möchten.

## Wie es geht

Das Erstellen einer temporären Datei in Python ist relativ einfach. Sie können die Standardbibliothek "tempfile" verwenden, um eine temporäre Datei mit dem Befehl "mkstemp()" zu erstellen. Hier ist ein Beispielcode:

``` Python
import tempfile 

# Erstellt eine temporäre Datei mit dem Präfix "myprefix_" 
temp_file = tempfile.mkstemp(prefix="myprefix_") 

# Schreibt Daten in die temporäre Datei 
with open(temp_file[1], 'w') as f: 
    f.write("Hier stehen die Daten")

# Gibt den Dateipfad der temporären Datei aus 
print("Temporäre Datei:", temp_file[1])

# Löscht die temporäre Datei 
temp_file[0].close() 
```

Dies würde eine Ausgabe ähnlich wie diese erzeugen:

```
Temporäre Datei: /var/folders/gk/r9nr343j6s7s4d_zgtztcv240000gn/T/myprefix_abcd1234
```

Wie Sie sehen, wird bei der Verwendung von "mkstemp()" sowohl ein Dateipfad als auch ein Dateideskriptor zurückgegeben. Der Dateipfad wird verwendet, um auf die temporäre Datei zuzugreifen und der Dateideskriptor wird verwendet, um die Datei zu schließen, wenn Sie fertig sind.

## Tiefere Einblicke

Während das Erstellen einer temporären Datei mit "mkstemp()" recht einfach ist, gibt es einige Dinge, die Sie beachten sollten. Zum Beispiel können Sie mit dem Argument "suffix" einen Suffix für den Dateinamen angeben und mit "dir" können Sie angeben, in welchem Verzeichnis die temporäre Datei erstellt werden soll. Zusätzlich können Sie "tempfile.NamedTemporaryFile()" verwenden, um eine benannte temporäre Datei mit einem leichter zugänglichen Dateiobjekt zu erstellen.

Es ist auch wichtig zu beachten, dass temporäre Dateien nicht automatisch gelöscht werden. Sie müssen sie manuell mit dem Dateideskriptor oder dem Dateipfad schließen und löschen. Wenn Sie die temporäre Datei nicht mehr benötigen, ist es eine gute Praxis, sie sofort zu löschen, damit Sie keine Dateien auf Ihrem System unnötig belassen.

## Siehe auch

- [Dokumentation zu "tempfile" in der Python-Standardbibliothek](https://docs.python.org/3/library/tempfile.html)
- [Ein ausführliches Tutorial zur Arbeit mit temporären Dateien in Python](https://www.blog.pythonlibrary.org/2014/01/17/how-to-create-and-delete-temporary-files-in-python/)