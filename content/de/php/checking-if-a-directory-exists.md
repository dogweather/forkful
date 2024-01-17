---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "PHP: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil der PHP-Programmierung. Es ermöglicht uns zu überprüfen, ob ein bestimmter Ordner vorhanden ist, bevor wir versuchen, auf ihn zuzugreifen. Dies hilft uns, Fehler zu vermeiden und unseren Code robust und fehlerfrei zu halten.

## Wie geht man vor?
Es gibt verschiedene Methoden, um zu überprüfen, ob ein Verzeichnis existiert. Eine Möglichkeit ist die Verwendung der `is_dir()`-Funktion, die einen booleschen Wert zurückgibt, der angibt, ob das angegebene Verzeichnis existiert oder nicht. Hier ist ein Beispiel, wie man dies in PHP verwenden könnte:

```PHP
<?php

$verzeichnis = "/home/benutzer/meinordner/";

if (is_dir($verzeichnis)) {
    echo "Das Verzeichnis existiert.";
} else {
    echo "Das Verzeichnis existiert nicht.";
}

?>
```

Die Ausgabe dieses Codes hängt davon ab, ob das angegebene Verzeichnis tatsächlich existiert oder nicht. Wenn das Verzeichnis existiert, wird "Das Verzeichnis existiert." ausgegeben, andernfalls wird "Das Verzeichnis existiert nicht." ausgegeben.

## Tiefere Einblicke
Die `is_dir()`-Funktion wurde erstmals in PHP 4.1 eingeführt und ist seitdem Teil der Kernsprache. Alternativ kann auch die `file_exists()`-Funktion verwendet werden, um zu überprüfen, ob ein Verzeichnis existiert oder nicht. Die Verwendung von `is_dir()` ist jedoch in diesem Fall vorzuziehen, da sie speziell für die Überprüfung auf Verzeichnisse entwickelt wurde.

In einigen Fällen kann es auch hilfreich sein, den absoluten Pfad zum Verzeichnis anzugeben, um sicherzustellen, dass das richtige Verzeichnis überprüft wird. Dies kann mit Hilfe der `realpath()`-Funktion erreicht werden, die den absoluten Pfad für eine gegebene Pfadangabe zurückgibt.

## Siehe auch
- [PHP-Dokumentation: is_dir()](https://www.php.net/manual/de/function.is-dir.php)
- [PHP-Dokumentation: file_exists()](https://www.php.net/manual/de/function.file-exists.php)
- [PHP-Dokumentation: realpath()](https://www.php.net/manual/de/function.realpath.php)