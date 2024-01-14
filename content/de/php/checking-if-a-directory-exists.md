---
title:                "PHP: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

### Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Aufgabe in der PHP-Programmierung. Es kann helfen, Fehler zu vermeiden und sicherzustellen, dass eine bestimmte Datei oder Ressource verfügbar ist, bevor darauf zugegriffen wird.

### Wie man es macht

Um zu überprüfen, ob ein Verzeichnis in PHP existiert, können Sie die Funktion `is_dir()` verwenden. Diese Funktion akzeptiert einen Parameter, der den Pfad zum zu überprüfenden Verzeichnis enthält. Wenn das Verzeichnis existiert, gibt die Funktion `true` zurück, andernfalls `false`.

````PHP
<?php
if (is_dir('/pfad/zum/verzeichnis')) {
    echo 'Das Verzeichnis existiert.';
} else {
    echo 'Das Verzeichnis existiert nicht.';
}
````

Wenn Sie den absoluten Pfad nicht kennen, können Sie auch den relativen Pfad verwenden, ausgehend von der aktuellen Arbeitsverzeichnisposition.

````PHP
<?php
if (is_dir('verzeichnis')) {
    echo 'Das Verzeichnis existiert.';
} else {
    echo 'Das Verzeichnis existiert nicht.';
}
````

### Tiefere Einblicke

Die Funktion `is_dir()` nutzt die Systemaufrufe `access()` oder `lstat()` unter der Haube, um zu überprüfen, ob ein Pfad eine gültige Verzeichnisliste ist. Je nach Dateisystem und Berechtigungen kann dies zu unterschiedlichen Ergebnissen führen. Es ist daher wichtig, sicherzustellen, dass die Berechtigungen ordnungsgemäß gesetzt sind, wenn Sie Probleme beim Überprüfen der Existenz eines Verzeichnisses haben.

Eine zusätzliche Möglichkeit, zu überprüfen, ob ein Verzeichnis existiert, besteht darin, die Funktion `file_exists()` zu verwenden und den Parameter `is_dir` auf `true` zu setzen.

````PHP
<?php
if (file_exists('pfad/zum/verzeichnis', true)) {
    echo 'Das Verzeichnis existiert.';
} else {
    echo 'Das Verzeichnis existiert nicht.';
}
````

### Siehe auch

- [PHP-Dokumentation für is_dir()](https://www.php.net/manual/de/function.is-dir.php)
- [PHP-Dokumentation für file_exists()](https://www.php.net/manual/de/function.file-exists.php)