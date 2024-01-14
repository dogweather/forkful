---
title:    "PHP: Überprüfen, ob ein Verzeichnis existiert."
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren ist es oft wichtig zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist. Das kann aus verschiedenen Gründen notwendig sein, zum Beispiel um sicherzustellen, dass ein Skript auf die benötigten Dateien zugreifen kann oder um eine bestimmte Aktion nur dann auszuführen, wenn das Verzeichnis existiert.

## Wie

Um in PHP zu überprüfen, ob ein Verzeichnis existiert, kann die Funktion `is_dir()` verwendet werden. Diese Funktion erwartet als Parameter den Pfad zu dem Verzeichnis, das überprüft werden soll. Wenn das Verzeichnis existiert, gibt die Funktion `true` zurück, andernfalls `false`.

```PHP
// Beispiel: Überprüfung des Verzeichnisses "bilder"
if (is_dir("bilder")) {
    echo "Das Verzeichnis existiert.";
} else {
    echo "Das Verzeichnis existiert nicht.";
}

// Ausgabe: "Das Verzeichnis existiert."
```

Eine andere Möglichkeit ist die Verwendung der Funktion `file_exists()`, die sowohl für Dateien als auch für Verzeichnisse funktioniert. Diese Funktion gibt ebenfalls `true` oder `false` zurück.

```PHP
// Beispiel: Überprüfung des Verzeichnisses "dokumente"
if (file_exists("dokumente")) {
    echo "Das Verzeichnis existiert.";
} else {
    echo "Das Verzeichnis existiert nicht.";
}

// Ausgabe: "Das Verzeichnis existiert nicht."
```

## Deep Dive

Wenn du tiefer in das Thema einsteigen möchtest, gibt es noch weitere Funktionen, die dir bei der Überprüfung von Verzeichnissen helfen können. Zum Beispiel gibt es die Funktion `scandir()`, die eine Liste aller Dateien und Verzeichnisse in einem bestimmten Pfad zurückgibt. Somit kannst du nicht nur überprüfen, ob ein Verzeichnis existiert, sondern auch auf die einzelnen Dateien und Verzeichnisse zugreifen.

```PHP
// Beispiel: Auflistung aller Dateien und Verzeichnisse in "bilder"
$files = scandir("bilder");

// Ausgabe: Array mit den Datei- und Verzeichnisnamen
print_r($files);
```

## Siehe auch

- [PHP Dokumentation - Verzeichnisse](https://www.php.net/manual/de/book.filesystem.php)
- [Codebeispiel: Dateien oder Verzeichnisse umbenennen mit PHP](https://www.tutdepot.com/change-the-name-of-file-or-folder-with-php/)
- [Video Tutorial: Einführung in PHP-Verzeichnisfunktionen](https://www.youtube.com/watch?v=hvGHLjkD7co)