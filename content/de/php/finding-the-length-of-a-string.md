---
title:                "PHP: Die Länge einer Zeichenkette finden."
simple_title:         "Die Länge einer Zeichenkette finden."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Warum: Stringlänge ist ein häufig verwendetes Konzept in der Programmierung. Es hilft uns, die Größe und Struktur eines Strings zu verstehen und die richtigen Manipulationen darauf durchzuführen.

Wie: Die Länge eines Strings kann mithilfe der integrierten Funktion `strlen()` in PHP gefunden werden. Diese Funktion nimmt den String als Parameter entgegen und gibt die Anzahl der Zeichen im String zurück. Hier ist ein Beispielcode, der die Verwendung von `strlen()` zeigt:

```PHP
<?php
$string = "Hallo Welt";
echo strlen($string); // Ausgabe: 10
```

Um die Länge eines Strings zu finden, müssen wir den String zunächst in einer Variablen speichern und dann `strlen()` auf diese Variable anwenden. Die Ausgabe des obigen Codes ist 10, da der String "Hallo Welt" insgesamt 10 Zeichen enthält.

Deep Dive: Die `strlen()` Funktion hat einige Besonderheiten, auf die man achten sollte. Zum Beispiel zählt sie nur die tatsächlichen Zeichen im String und nicht die Leerzeichen oder Sonderzeichen. Das bedeutet, dass in einem String wie "Hallo Welt!" die Länge nur 11 beträgt, obwohl der String insgesamt 12 Zeichen enthält.

Außerdem muss beachtet werden, dass `strlen()` die Länge des Strings auch in Bezug auf die verwendete Codierung berechnet. Dies kann zu Unterschieden in der Länge führen, abhängig davon, ob der String in UTF-8 oder einer anderen Codierung vorliegt.

Siehe auch:
- Offizielle PHP-Dokumentation zu `strlen()`: https://www.php.net/manual/de/function.strlen.php
- Ein ausführliches Tutorial zu Strings in PHP: https://phpentwickler.de/string-benutzung-php.html
- Praktische Beispiele zur Verwendung von `strlen()`: https://www.php-kurs.com/stringlaenge.htm