---
title:                "PHP: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der PHP-Programmierung gibt es oft die Notwendigkeit, temporäre Dateien zu erstellen. Dies kann aus verschiedenen Gründen geschehen, wie zum Beispiel zum Zwischenspeichern von Daten oder zum Erstellen von Sicherungskopien. In diesem Artikel werden wir uns ansehen, wie man in PHP temporäre Dateien erstellen kann und warum es eine nützliche Fähigkeit ist.

## Wie man es macht

Die Erstellung einer temporären Datei in PHP kann recht einfach erfolgen. Hier ist ein Beispielcode mit einem Kommentar, der den Prozess erklärt:

```PHP
// Hier wird ein eindeutiger Dateiname generiert, der aus einem Präfix und einer zufälligen Zahl besteht
$filename = tempnam(sys_get_temp_dir(), 'prefix_');

// Jetzt kann die Datei geöffnet und beschrieben werden
$file = fopen($filename, "w");
fwrite($file, "Hier ist der Inhalt der temporären Datei");
fclose($file);

// Der Inhalt kann nun ausgegeben werden, um zu überprüfen, dass die Datei erfolgreich erstellt wurde
echo file_get_contents($filename);
```

Die Ausgabe der oben genannten Beispiele wird folgendes sein:

```
Hier ist der Inhalt der temporären Datei
```

## Tiefere Einblicke

Jetzt, wo wir wissen, wie man in PHP eine temporäre Datei erstellt, schauen wir uns einige wichtige Informationen dazu an. Die `tempnam()` Funktion generiert einen eindeutigen Dateinamen und erstellt gleichzeitig die Datei. Sie akzeptiert zwei Parameter: das Verzeichnis, in dem die Datei erstellt werden soll, und ein Präfix, das dem Dateinamen vorangestellt wird.

Es gibt auch eine andere Funktion, `tmpfile()`, die eine temporäre Datei erstellt und direkt öffnet. Diese Funktion ist besonders nützlich, wenn Sie die Datei direkt beschreiben möchten. Hier ist ein Beispiel:

```PHP
// Eine temporäre Datei wird erstellt und geöffnet
$file = tmpfile();

// Schreibt etwas in die Datei
fwrite($file, "Hier ist der Inhalt der temporären Datei");

// Der Inhalt wird ausgegeben
echo fread($file, 54);

// Die Datei wird geschlossen und gelöscht
fclose($file);
```

Die Ausgabe wird folgendes sein:

```
Hier ist der Inhalt der temporären Datei
```

Man muss allerdings beachten, dass die `tmpfile()` Funktion die Datei sofort öffnet, wodurch sie unabhängig von der `fclose()` Funktion nicht automatisch gelöscht wird. Sie müssen die Datei selbst mit der `unlink()` Funktion löschen.

## Siehe auch

- PHP-Dokumentation zu [tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- PHP-Dokumentation zu [tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- W3Schools-Artikel über [Temporäre Dateien in PHP](https://www.w3schools.com/php/php_ref_filesystem.asp)