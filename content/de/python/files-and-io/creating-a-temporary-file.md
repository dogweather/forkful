---
title:                "Erstellung einer temporären Datei"
aliases:
- /de/python/creating-a-temporary-file.md
date:                  2024-01-20T17:40:55.011294-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Temporäre Dateien sind kurzlebige Datenbehälter, die während der Laufzeit eines Programms für Datentransfer, Zwischenspeicherungen oder Sicherheitszwecke genutzt werden. Programmierer verwenden sie, um den Arbeitsspeicher zu entlasten, Datenkonflikte zu vermeiden und um sensible Informationen nicht dauerhaft zu speichern.

## So geht's:
```Python
import tempfile

# Temporäre Datei erstellen und schreiben
with tempfile.NamedTemporaryFile(mode='w+t', delete=False) as temp_file:
    temp_file.write('Hallo, Welt!')
    
    # Zurück zum Anfang der Datei springen und lesen
    temp_file.seek(0)
    print(temp_file.read())

# Temporäre Datei-Pfad ausgeben
print("Temporäre Datei erstellt unter:", temp_file.name)

# Temporäre Datei löschen
import os
os.unlink(temp_file.name)
print("Temporäre Datei gelöscht.")
```

Ausgabe:
```
Hallo, Welt!
Temporäre Datei erstellt unter: /tmp/tmpabcdefg12345
Temporäre Datei gelöscht.
```

## Tiefgang:
In der Vergangenheit mussten Programmierer oft manuell temporäre Dateien verwalten, was zu Sicherheits- und Speicherproblemen führen könnte. Die Verwendung von `tempfile` in Python vereinfacht diesen Vorgang, indem es automatisch einzigartige Dateinamen generiert und Optionen zur selbständigen Bereinigung bietet.

Alternativen zum `tempfile`-Modul sind das direkte Arbeiten im Speicher mit `io.BytesIO` oder `io.StringIO`, wenn die Datei nicht zu groß ist. Für permanente Datenspeicherung kann auf normale Dateioperationen zurückgegriffen werden.

Implementierungsdetails umfassen, dass `NamedTemporaryFile` unter bestimmten Betriebssystemen, wie Windows, nicht immer wie erwartet funktioniert, da das Betriebssystem das gleichzeitige Öffnen von Dateien, die zum Löschen markiert sind, verhindern kann. Das Argument `delete=False` kann diese Probleme umgehen, erfordert aber, dass die Datei manuell gelöscht wird.

## Siehe auch:
- Python-Dokumentation zum `tempfile`-Modul: https://docs.python.org/3/library/tempfile.html
- Python-Dokumentation zu `io.StringIO` und `io.BytesIO`: https://docs.python.org/3/library/io.html
- Sicherheitsaspekte temporärer Dateien: https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File
