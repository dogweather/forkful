---
title:    "PHP: Umwandeln einer Zeichenfolge in Kleinbuchstaben"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum

In der Welt der PHP-Programmierung gibt es oft die Notwendigkeit, Strings in Kleinbuchstaben umzuwandeln. Dies kann aus unterschiedlichen Gründen erforderlich sein, wie zum Beispiel zur besseren Datenkonsistenz oder zur Ausgabe in einem bestimmten Format.

## Wie geht man vor?

Um einen String in PHP in Kleinbuchstaben umzuwandeln, gibt es eine sehr einfache und praktische Funktion, die man verwenden kann: `strtolower()`. Diese Funktion akzeptiert einen String als Argument und gibt den gleichen String, jedoch komplett in Kleinbuchstaben, zurück.

```PHP
$text = "Die Sonne scheint in Deutschland!";
echo strtolower($text);
```
Das obige Beispiel würde folgenden Output erzeugen:
```HTML
die sonne scheint in deutschland!
```

Weitere Beispiele und Informationen zu dieser Funktion finden Sie in der [PHP-Dokumentation](https://www.php.net/manual/de/function.strtolower.php).

## Tiefergehende Information

Es gibt auch andere Möglichkeiten, einen String in PHP in Kleinbuchstaben zu konvertieren. Eine davon ist die Verwendung der `mb_strtolower()`-Funktion, die für Texte mit mehreren Bytes, wie z.B. mit Umlauten, besser geeignet ist. Diese Funktion verwendet die Anweisungen des aktuellen Zeichensatzes, um den String korrekt umzuwandeln.

Eine andere Möglichkeit ist die Verwendung des `lcfirst()`-Befehls, der den ersten Buchstaben eines Strings in Kleinbuchstaben umwandelt, jedoch die restlichen Buchstaben unverändert lässt.

Es ist wichtig zu beachten, dass die `strtolower()`-Funktion keine Akzente entfernt, sondern nur die Buchstaben in Kleinbuchstaben umwandelt. Wenn Sie also einen String ohne Umlaute und Sonderzeichen benötigen, sollten Sie die `str_replace()`-Funktion verwenden, um diese Zeichen zu entfernen.

## Siehe auch

* [PHP-Dokumentation: strtolower()](https://www.php.net/manual/de/function.strtolower.php)
* [PHP-Dokumentation: mb_strtolower()](https://www.php.net/manual/de/function.mb-strtolower.php)
* [PHP-Dokumentation: lcfirst()](https://www.php.net/manual/de/function.lcfirst.php)