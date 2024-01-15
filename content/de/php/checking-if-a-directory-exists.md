---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist"
html_title:           "PHP: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum Sie vielleicht überprüfen möchten, ob ein Verzeichnis in Ihrem PHP-Code existiert. Zum Beispiel können Sie sicherstellen, dass ein bestimmtes Verzeichnis für das Speichern von Dateien vorhanden ist, bevor Sie versuchen, Daten in dieses Verzeichnis zu schreiben.

## Wie geht das?

Um zu überprüfen, ob ein Verzeichnis in PHP existiert, können Sie die Funktion `is_dir()` verwenden. Diese Funktion gibt `true` zurück, wenn das angegebene Verzeichnis existiert, oder `false`, wenn es nicht existiert.

Hier ist ein Beispiel, das überprüft, ob ein Verzeichnis namens "uploads" existiert und je nach Ergebnis eine entsprechende Meldung ausgibt:

```PHP
if (is_dir("uploads")) {
    echo "Das Verzeichnis existiert.";
} else {
    echo "Das Verzeichnis existiert nicht.";
}
```

Diese Funktion kann auch verwendet werden, um zu überprüfen, ob ein Verzeichnis schreibbar ist, indem Sie `is_writable()` verwenden.

```PHP
if (is_writable("uploads")) {
    echo "Das Verzeichnis ist schreibbar.";
} else {
    echo "Das Verzeichnis ist nicht schreibbar.";
}
```

## Tiefergehende Informationen

Wenn Sie genauer untersuchen möchten, warum ein Verzeichnis nicht existiert oder nicht schreibbar ist, können Sie die Funktion `file_exists()` verwenden, um zu überprüfen, ob es sich bei dem angegebenen Pfad um ein Verzeichnis handelt.

```PHP
$path = "uploads";

if (file_exists($path) && is_dir($path)) {
    echo "Das Verzeichnis existiert.";
    if (is_writable($path)) {
        echo "Das Verzeichnis ist schreibbar.";
    }
} else {
    echo "Das Verzeichnis existiert nicht oder ist kein Verzeichnis.";
}
```

Sie können auch den absoluten Pfad angeben, anstatt nur den Namen des Verzeichnisses zu verwenden, um sicherzustellen, dass Sie das richtige Verzeichnis überprüfen, insbesondere wenn Sie mit mehreren Verzeichnissen arbeiten.

## Siehe auch

- [PHP-Dokumentation zu `is_dir()`](https://www.php.net/manual/de/function.is-dir.php)
- [PHP-Dokumentation zu `is_writable()`](https://www.php.net/manual/de/function.is-writable.php)
- [PHP-Dokumentation zu `file_exists()`](https://www.php.net/manual/de/function.file-exists.php)