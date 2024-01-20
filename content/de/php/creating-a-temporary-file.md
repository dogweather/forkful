---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# **Das Erzeugen von temporären Dateien in PHP**

## **Was & Warum?**

Temporäre Dateien sind vorübergehende Speicher, die für kurzfristige Datenspeicherung und -manipulation genutzt wird. Sie sind nützlich, wenn große Datenmengen bearbeitet werden und der Arbeitsspeicher knapp ist.

## **Anleitung:**

Mit PHP können Sie temporäre Dateien mit der Funktion `tmpfile()` erstellen. Hier ist ein Beispiel:

```PHP
<?php
$temps = tmpfile();
fwrite($temps, "Wir schreiben Daten");
rewind($temps);

// Wir lesen die Daten zurück
echo fread($temps, 1024);  // Gibt 'Wir schreiben Daten' aus
fclose($temps); // Die temporäre Datei wird geschlossen und gelöscht
?>
```

## **Hintergrundwissen**

Temporäre Dateien wurden ursprünglich als Lösung für Speicherprobleme eingeführt. Anstatt Daten ständig im Speicher zu halten, ermöglichen temporäre Dateien das Speichern von Daten auf einer Festplatte, was effektiver ist. 

Es gibt auch andere Möglichkeiten, temporäre Dateien zu erstellen, wie `tempnam()`, die Ihnen mehr Kontrolle bietet, aber es erfordert auch, die Datei manuell zu löschen. 

Die `tmpfile()` Funktion erstellt eine Datei mit einem einzigartigen Namen im temporären Verzeichnis des Systems. Die Datei wird automatisch gelöscht, wenn sie geschlossen wird (etwa wenn das Skript endet), oder wenn das Skript, das sie erzeugt hat, beendet wird.

## **Weiterführende Links**

1. [PHP: tmpfile - Manual](https://www.php.net/manual/en/function.tmpfile.php)
2. [PHP: tempnam - Manual](https://www.php.net/manual/en/function.tempnam.php)