---
title:    "PHP: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Funktion in der PHP-Programmierung. Es ermöglicht es uns, sicherzustellen, dass unsere Dateien an den richtigen Speicherort gespeichert werden und dass unser Programm fehlerfrei bleibt.

## Wie geht man vor?

Die Überprüfung eines Verzeichnisses in PHP ist relativ einfach und kann mit wenigen Codezeilen erreicht werden. Sehen wir uns ein Beispiel an:

```
<?php
// Verzeichnisname festlegen
$verzeichnis = "meinVerzeichnis";

// Überprüfen, ob das Verzeichnis existiert
if (file_exists($verzeichnis)) {
   echo "Das Verzeichnis $verzeichnis existiert bereits.";
} else {
   echo "Das Verzeichnis $verzeichnis existiert noch nicht.";
}
?>
```

Verwenden Sie die Funktion `file_exists()`, um festzustellen, ob das Verzeichnis existiert. Wenn die Bedingung erfüllt ist, wird die erste Ausgabe ausgeführt, wenn nicht, dann die zweite.

Sie können auch die Funktion `is_dir()` verwenden, um zu überprüfen, ob es sich bei dem angegebenen Pfad um ein Verzeichnis handelt. Hier ist ein Beispielcode:

```
<?php
// Verzeichnisname festlegen
$verzeichnis = "meinVerzeichnis";

// Überprüfen, ob es sich um ein Verzeichnis handelt
if (is_dir($verzeichnis)) {
   echo "Der angegebene Pfad ist ein Verzeichnis.";
} else {
   echo "Der angegebene Pfad ist kein Verzeichnis.";
}
?>
```

## Tiefere Einblicke

Es ist auch möglich, mithilfe der Funktion `scandir()` alle Dateien und Unterverzeichnisse in einem Verzeichnis aufzulisten. Hier ist ein Beispielcode:

```
<?php
// Verzeichnisname festlegen
$verzeichnis = "meinVerzeichnis";

// Liste aller Dateien und Unterverzeichnisse ausgeben
$files = scandir($verzeichnis);

// Ausgabe formatieren
foreach ($files as $file) {
   echo $file . "<br>";
}
?>
```

Dieser Code scannt das angegebene Verzeichnis und gibt eine Liste aller darin enthaltenen Dateien und Verzeichnisse aus.

## Siehe auch

- "How to Create, Rename, Move and Delete Directories in PHP" (https://www.w3schools.com/php/php_ref_directory.asp)
- "PHP Filesystem Functions" (https://www.php.net/manual/en/ref.filesystem.php)
- "Handling File Uploads with PHP" (https://www.tutorialspoint.com/php/php_file_uploading.htm)

Mithilfe dieser Funktionen und Beispiele können Sie in der PHP-Programmierung problemlos überprüfen, ob ein Verzeichnis existiert. Wir hoffen, dass dieser Artikel hilfreich war und Ihre Programmierung erleichtert!