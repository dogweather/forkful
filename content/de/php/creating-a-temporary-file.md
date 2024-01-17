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

## Was & Warum?

Das Erstellen einer temporären Datei ist ein häufiges Verfahren in der Programmierung, bei dem eine Datei für einen begrenzten Zeitraum erstellt und verwendet wird. Programmierer tun dies, um temporäre Daten zu speichern, die während der Laufzeit eines Programms benötigt werden, aber nicht dauerhaft gespeichert werden müssen. Es ist auch nützlich, um sicherzustellen, dass verschiedene Programme oder Prozesse nicht auf die gleiche Datei zugreifen und Daten überschreiben.

## So geht's:

Die Erstellung einer temporären Datei in PHP ist einfach. Zunächst müssen wir die Funktion ```tempnam()``` verwenden, um einen Pfad und einen Dateinamen für die temporäre Datei zu generieren. Dann können wir die Datei mit der Funktion ```fopen()``` öffnen und schreiben. Hier ist ein Beispielcode mit einer Ausgabe:

```PHP
<?php

// Erstelle eine temporäre Datei mit der Endung ".tmp"
$temp_file = tempnam('/tmp', 'testfile');

// Öffne und schreibe in die Datei
$file_handle = fopen($temp_file, 'w');
fwrite($file_handle, 'Dies ist eine Testdatei');
fclose($file_handle);

// Gib den Pfad und Dateinamen der temporären Datei aus
echo $temp_file;
?>
```

Die Ausgabe dieses Codes wäre beispielsweise: ```/tmp/testfileg0er1B.tmp```. Beachten Sie, dass die Dateiendung ".tmp" automatisch von der Funktion ```tempnam()``` hinzugefügt wird. Um die Sicherheit zu erhöhen, empfiehlt es sich, einen Zufallsstring als Dateipräfix zu verwenden, anstatt wie im obigen Beispiel den Dateinamen "testfile" anzugeben.

## Tiefere Einblicke:

Die Verwendung von temporären Dateien hat eine lange Geschichte in der Softwareentwicklung. Früher wurden sie häufig genutzt, um Zwischenergebnisse oder temporäre Daten zu speichern, die für die Ausführung eines Programms erforderlich waren. Jedoch gibt es heutzutage auch alternative Methoden, wie die Verwendung von Speicherstrukturen oder Datenbanken, um temporäre Daten effizient zu verwalten.

Die Funktion ```tempnam()``` ist nicht die einzige Möglichkeit, temporäre Dateien in PHP zu erstellen. Es gibt auch die Funktionen ```tmpfile()``` und ```tmpnam()```, die jedoch jeweils ihre eigenen Besonderheiten haben. Zum Beispiel wird bei ```tmpfile()``` die temporäre Datei automatisch erstellt, geöffnet und ein Dateihandle zurückgegeben, während bei ```tmpnam()``` nur der Dateipfad und -name generiert wird. Es ist wichtig, die richtige Funktion entsprechend den Anforderungen des Programms auszuwählen.

Die Erstellung von temporären Dateien ist auch ein wichtiger Aspekt der Sicherheit im Programmierbereich. Es ist wichtig, sicherzustellen, dass temporäre Dateien korrekt behandelt und nach der Verwendung richtig gelöscht werden, um mögliche Sicherheitslücken zu vermeiden.

## Siehe auch:

- [PHP: tempnam() function](https://www.php.net/manual/de/function.tempnam.php)
- [PHP: tmpfile() function](https://www.php.net/manual/de/function.tmpfile.php)
- [PHP: tmpnam() function](https://www.php.net/manual/de/function.tmpnam.php)