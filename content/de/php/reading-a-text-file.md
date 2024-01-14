---
title:                "PHP: Das Lesen einer Textdatei"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es unzählige Aufgaben, die erledigt werden müssen. Eine davon ist das Lesen von Textdateien. Dies kann aus verschiedenen Gründen notwendig sein, wie zum Beispiel das Verarbeiten von Benutzereingaben oder das Lesen von Konfigurationsdateien. In diesem Blog-Beitrag werden wir uns darauf konzentrieren, wie man eine Textdatei in PHP liest und welche Möglichkeiten es dabei gibt.

## Wie geht's?

Um eine Textdatei in PHP zu lesen, gibt es verschiedene Funktionen, aber die grundlegende Methode ist die Verwendung der Funktion `file_get_contents()`. Diese Funktion liest den Inhalt einer Datei in einen String und gibt ihn zurück. Hier ist ein Beispielcode:

```PHP
$text = file_get_contents("mein_text.txt");
echo $text;
```

Die Funktion `file_get_contents()` akzeptiert auch eine optionale Parameter, um den Lesevorgang zu steuern, wie zum Beispiel die Anzahl der zu lesenden Byte oder die Startposition im Dateiinhalt. Um mehr über diese Optionen zu erfahren, können Sie die Dokumentation von PHP zu Rate ziehen.

Es gibt auch andere Funktionen, die verwendet werden können, um eine Textdatei in PHP zu lesen, wie zum Beispiel `fopen()` und `fgets()`. Diese können jedoch komplizierter sein, daher ist die Verwendung von `file_get_contents()` oft die einfachste Option.

## Tief in die Materie eintauchen

Ein wichtiger Aspekt beim Lesen von Textdateien ist die Kodierung. Wenn die Datei in einer anderen Kodierung als der Standardsprache des Servers gespeichert wird, kann es zu Problemen beim Lesen der Datei kommen. In diesem Fall müssen Sie die Funktion `mb_convert_encoding()` verwenden, um den Dateiinhalt in die gewünschte Kodierung zu konvertieren.

Eine weitere Sache, die Sie beim Lesen von Textdateien beachten sollten, sind Umbrüche. In Windows werden mithilfe des Zeichenkodes `"\r\n"` Umbrüche erzeugt, während in Linux und Mac OS `"\n"` verwendet wird. Wenn Sie sicherstellen möchten, dass Sie den Dateiinhalt richtig lesen, müssen Sie vielleicht die Funktion `str_replace()` verwenden, um diese Umbrüche zu vereinheitlichen.

## Siehe auch

- [PHP: Lesen von Dateien](https://www.php.net/manual/de/function.file-get-contents.php)
- [PHP: mb_convert_encoding() Funktion](https://www.php.net/manual/de/function.mb-convert-encoding.php)
- [PHP: str_replace() Funktion](https://www.php.net/manual/de/function.str-replace.php)