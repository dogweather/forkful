---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei ist der Prozess, bei dem ein Programm den Inhalt einer Textdatei abruft und interpretiert. Programmierer tun dies, um Daten aus einer Datei zu extrahieren und sie in ihre Anwendung zu integrieren.

## So geht's:

```PHP
<?php
$dateiname = 'textdatei.txt';

if (!file_exists($dateiname)) {
    die("Datei nicht gefunden");
}

$inhalt = file_get_contents($dateiname);

echo $inhalt;
?>
```

Dieser Code sucht nach einer Datei namens 'textdatei.txt' und liest ihren Inhalt. Wenn die Datei nicht existiert, wird das Skript mit einer Fehlermeldung beendet.

## Tiefere Einblicke 

Historisch gesehen war das Lesen von Textdateien eine der ersten Möglichkeiten, Daten persistent in Computersystemen zu speichern. In PHP bietet die Funktion `file_get_contents()` eine einfache Möglichkeit, eine Textdatei zu lesen.

Es gibt jedoch Alternativen. Man kann auch `fopen()` und `fread()` verwenden, wenn mehr Kontrolle über den Lesevorgang benötigt wird. Ein wichtiger Implementierungsdetail ist das Fehlerhandling. Im obigen Beispiel verwenden wir `die()`, um das Skript zu beenden, wenn die Datei nicht vorhanden ist.

## Siehe Auch

Weitere Informationen finden Sie in der offiziellen PHP-Dokumentation: [Dateisystem Funktionen](https://www.php.net/manual/de/book.filesystem.php).