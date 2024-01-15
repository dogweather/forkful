---
title:                "Erstellen einer temporären Datei"
html_title:           "PHP: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist eine nützliche Technik in der PHP-Programmierung, um Daten vorübergehend zu speichern und zu verarbeiten, ohne das eigentliche System zu beeinträchtigen. Dies kann hilfreich sein, wenn zum Beispiel große Datenmengen verarbeitet werden müssen oder es Verbindungsprobleme mit externen Ressourcen gibt.

## Wie geht das?

Das Erstellen einer temporären Datei in PHP ist relativ einfach. Wir verwenden die Funktion `tmpfile()`, die uns ein Objekt vom Typ `resource` zurückgibt. Dieses Objekt kann dann wie eine normale Datei behandelt werden.

```PHP
<?php
$temp_file = tmpfile(); // Erstellt eine temporäre Datei
fwrite($temp_file, "Dies ist ein Test"); // Schreibt Daten in die Datei
echo fread($temp_file, 4096); // Gibt die Daten aus der Datei aus
fclose($temp_file); // Schließt die Datei und löscht sie automatisch
```

Das obige Beispiel zeigt, wie wir eine temporäre Datei erstellen, Daten in sie schreiben und dann lesen und schließen können. Beachten Sie, dass die temporäre Datei automatisch gelöscht wird, wenn sie geschlossen wird, so dass Sie sich keine Gedanken darüber machen müssen.

## Tiefer eintauchen

Neben der oben gezeigten einfachen Verwendung gibt es auch einige Optionen, die Sie bei der Verwendung von `tmpfile()` berücksichtigen sollten. Zum Beispiel werden standardmäßig die temporären Dateien im Temp-Verzeichnis des Systems gespeichert. Sie können jedoch auch einen benutzerdefinierten Pfad festlegen, indem Sie `sys_get_temp_dir()` verwenden und das Ergebnis als Parameter an `tmpfile()` übergeben.

```PHP
<?php
$temp_path = './temp/'; // Benutzerdefinierter Pfad
$temp_file = tmpfile(sys_get_temp_dir()); // Verwenden Sie den benutzerdefinierten Pfad
```

Darüber hinaus können Sie auch die Größe der temporären Datei begrenzen, indem Sie als zweiten Parameter in `tmpfile()` die maximale Größe in Bytes angeben.

## Siehe auch

Für weitere Informationen über das Erstellen von temporären Dateien in PHP empfehle ich die offizielle Dokumentation:

- [PHP: tmpfile - Manual](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP: sys_get_temp_dir - Manual](https://www.php.net/manual/en/function.sys-get-temp-dir.php)