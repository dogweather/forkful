---
title:                "PHP: Eine temporäre Datei erstellen"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen temporärer Dateien ist ein wichtiger Teil des Programmierens mit PHP. Sie ermöglichen es, temporäre Daten oder Zwischenschritte in einem Programm zu speichern, ohne dass diese dauerhaft auf dem Server existieren müssen. Dadurch können Ressourcen gespart und Prozesse optimiert werden.

## Wie geht das?

Um eine temporäre Datei in PHP zu erstellen, können wir die function `tmpfile()` verwenden. Diese Funktion erzeugt eine temporäre Datei mit einer eindeutigen Bezeichnung, öffnet diese und gibt einen Zeiger darauf zurück. Hier ein Beispiel:

```PHP
$tempFile = tmpfile();
fwrite($tempFile, "Hallo Welt!");
echo fread($tempFile, 11); // Output: "Hallo Welt!"
fclose($tempFile); // Die temporäre Datei wird automatisch gelöscht
```

In diesem Beispiel wird eine temporäre Datei erstellt, in diese der String "Hallo Welt!" geschrieben wird, und anschließend wird der Inhalt der Datei ausgelesen und ausgegeben. Am Ende wird die Datei automatisch gelöscht, da der Zeiger darauf geschlossen wird.

## Tiefere Einblicke

Das Erstellen von temporären Dateien kann auch mit der Funktion `tempnam()` erfolgen. Diese ermöglicht es, einen spezifischen Dateinamen für die temporäre Datei anzugeben. Zum Beispiel:

```PHP
$tempFile = tempnam("temp", "prefix");
echo $tempFile; // Output: "temp/prefix.rnd"
```

Hier wird eine temporäre Datei im Ordner "temp" mit dem Präfix "prefix" erstellt. Diese Funktion gibt den erzeugten Dateinamen zurück, ohne die Datei zu öffnen. Es ist wichtig, darauf zu achten, dass der Ordner, in dem die temporäre Datei erstellt werden soll, existiert und der Server Schreibzugriff darauf hat.

## Siehe auch

- [PHP Dokumentation zu tmpfile()](https://www.php.net/manual/de/function.tmpfile.php)
- [PHP Dokumentation zu tempnam()](https://www.php.net/manual/de/function.tempnam.php)
- [Tutorial zu temporären Dateien mit PHP](https://www.php-einfach.de/experte/temporaere-dateien/)