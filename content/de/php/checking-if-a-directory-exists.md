---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:57:44.748331-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
PHP-Entwickler prüfen, ob Verzeichnisse existieren, um Fehler zu vermeiden und zu entscheiden, ob sie Aktionen wie das Lesen von Dateien oder das Erstellen neuer Dateien darin vornehmen müssen. Es ist essenziell für robuste Dateisystemoperationen.

## So geht's:
Mit `is_dir()` prüfen wir, ob ein bestimmtes Verzeichnis existiert. Die Funktion `mkdir()` erstellt ein neues Verzeichnis.

```php
<?php
$directory = "/mein/verzeichnis";

// Prüfen, ob das Verzeichnis existiert
if (is_dir($directory)) {
    echo "Das Verzeichnis existiert bereits.";
} else {
    // Erstellen Sie das Verzeichnis, weil es nicht existiert
    if (mkdir($directory, 0777, true)) {
        echo "Das Verzeichnis wurde erstellt.";
    } else {
        echo "Fehler beim Erstellen des Verzeichnisses.";
    }
}
?>
```

## Tiefgang
Das Prüfen von Verzeichnissen ist seit den ersten PHP-Versionen möglich. Früher gab es Funktionen wie `file_exists()`, die auch für Verzeichnisse verwendet wurden, aber `is_dir()` ist spezifisch und zuverlässiger. Es gibt auch andere Funktionen wie `scandir()`, die einen Fehler ausgeben, wenn das Verzeichnis nicht existiert, wodurch auch indirekt die Existenz geprüft werden kann. Beim Erstellen von Verzeichnissen mit `mkdir()` empfiehlt es sich, die Zugriffsrechte sorgfältig zu wählen, um Sicherheitsrisiken zu vermeiden.

## Siehe auch
- [PHP Handbuch zu `is_dir()`](https://www.php.net/manual/de/function.is-dir.php)
- [PHP Handbuch zu `mkdir()`](https://www.php.net/manual/de/function.mkdir.php)
- [PHP Handbuch zu `file_exists()`](https://www.php.net/manual/de/function.file-exists.php)
