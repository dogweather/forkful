---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Go: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Überprüfung, ob ein Verzeichnis existiert, ist ein sehr nützliche Methode, die verwendet wird, um zu sehen, ob ein bestimmter Ordnerpfad in Ihrem System vorhanden ist. Programmierer nutzen es, um Fehler zu vermeiden, die sich ergeben können, wenn versucht wird, auf ein Verzeichnis zuzugreifen, das nicht existiert.

## So geht's:

In PHP können wir die Funktion `is_dir()` verwenden, um zu überprüfen, ob ein Verzeichnis existiert oder nicht. Hier ist ein einfaches Beispiel:

```PHP
<?php

$dirPath = 'mein/verzeichnis/pfad';

if(is_dir($dirPath)){
    echo "Das Verzeichnis existiert";
} else {
    echo "Das Verzeichnis existiert nicht";
}

?>
```

Im obigen Beispiel wird `is_dir()` mit einem Verzeichnispfad als Parameter aufgerufen. Es gibt "Das Verzeichnis existiert" zurück, wenn es das Verzeichnis finde, und "Das Verzeichnis existiert nicht" ansonsten.

## Tiefere Einblicke:

Die Funktion `is_dir()` wurde bereits in PHP 4.0.0 eingeführt und ist seitdem eine verlässliche Methode zur Überprüfung der Existenz eines Verzeichnisses. Alternativ können wir auch die `file_exists()` Funktion verwenden, die auch bei Dateien funktioniert, nicht nur bei Ordnern.

Es ist wichtig zu wissen, dass `is_dir()` nur einen booleschen Wert zurückgibt. Daher kann diese Funktion nicht verwendet werden, um weitere Informationen über das Verzeichnis zu erhalten. Wenn man mehr Details braucht, sollte man auf Funktionen wie `fileperms()` oder `filemtime()` zurückgreifen.

Außerdem, Funktionen wie `is_dir()` oder `file_exists()` ignorieren möglicherweise Standard Linux/Unix Aliase wie '~'. Daher sollte man absolute Pfadnamen verwenden.

## Siehe auch:

Für mehr Einblicke in das Dateisystem von PHP, besuchen Sie bitte:

1. Die offizielle PHP-Dokumentation zur Verzeichnisbehandlung: https://www.php.net/manual/de/book.dir.php
2. Die PHP-Dokumentation über `is_dir()`: https://www.php.net/manual/de/function.is-dir.php
3. Stack Overflow Diskussionen über die `is_dir()` Funktion: https://stackoverflow.com/questions/3137094/how-to-check-if-a-directory-exists-in-php