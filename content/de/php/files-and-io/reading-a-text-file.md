---
title:                "Textdatei einlesen"
date:                  2024-01-20T17:55:05.030177-07:00
model:                 gpt-4-1106-preview
simple_title:         "Textdatei einlesen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen einer Textdatei in PHP bedeutet, den Inhalt der Datei Zeile für Zeile oder als Ganzes in deinem Skript verfügbar zu machen. Programmierer müssen das tun, um Daten zu verarbeiten, Einstellungen zu laden oder einfach Informationen auszutauschen.

## Anleitung:
Ein einfaches Skript, um eine Datei namens "beispiel.txt" zu lesen:

```php
<?php
$filename = 'beispiel.txt';

// Inhalt als einen String lesen
$inhalt = file_get_contents($filename);
echo $inhalt;

// Oder jede Zeile in einem Array lesen
$zeilen = file($filename);
foreach ($zeilen as $zeile) {
    echo $zeile;
}
?>
```

Ausgabe, je nach Inhalt der "beispiel.txt":

```
Hallo, ich bin eine Textzeile.
Und ich bin die zweite Zeile!
```

## Tiefenblick:
In den Anfängen von PHP war das Lesen einer Datei oft mit `fopen()` und `fread()` verbunden, Funktionen, die heute immer noch nützlich sind, wenn man mehr Kontrolle braucht, zum Beispiel beim Lesen großer Dateien. Alternativen wie `file_get_contents()` und `file()` sind komfortabler für vollständiges oder zeilenweises Lesen. 

Beim Umgang mit Dateien solltest du immer Fehlerbehandlung berücksichtigen. Ein fehlender Zugriff oder ein nicht existierender Dateipfad kann durch Verwendung von `file_exists()` und `is_readable()` vor dem eigentlichen Lesen geprüft werden.

Die verschiedenen Lese-Methoden haben auch unterschiedliche Auswirkungen auf die Performance, besonders bei großen Dateien. `file_get_contents()` liest die gesamte Datei auf einmal in den Speicher, während `fopen()` in Verbindung mit `fread()` ermöglicht, stückweise zu lesen.

## Siehe Auch:
- Die offizielle PHP-Dokumentation zu `file_get_contents()`: https://www.php.net/manual/de/function.file-get-contents.php
- PHP.net zu `file()`: https://www.php.net/manual/de/function.file.php
- Ein Diskussionsfaden zu den Performance-Unterschieden zwischen `file_get_contents()` und `fopen()`: https://stackoverflow.com/questions/147821/loading-file-contents-to-a-variable-vs-output-buffering
