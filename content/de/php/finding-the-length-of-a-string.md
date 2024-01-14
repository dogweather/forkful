---
title:                "PHP: Die Länge einer Zeichenkette finden."
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Operation in der Programmierung. Es ist wichtig, um die Anzahl der Zeichen in einem String zu kennen, was in vielen Anwendungsfällen hilfreich sein kann.

## Wie geht man vor

Um die Länge eines Strings in PHP zu finden, gibt es verschiedene Ansätze. Eine Möglichkeit ist die Verwendung der eingebauten PHP-Funktion `strlen()`. Diese Funktion gibt die Anzahl der Zeichen in einem String zurück. Zum Beispiel:

```PHP
$string = "Hallo";
echo strlen($string); // Ausgabe: 5
```

Eine weitere Möglichkeit ist die Verwendung der `mb_strlen()` Funktion, die spezielle Zeichen unterstützt, die aus mehreren Bytes bestehen. Zum Beispiel:

```PHP
$string = "Großartig!"; // enthält das spezielle Zeichen "ß"
echo mb_strlen($string); // Ausgabe: 9
```

## In die Tiefe gehen

Es gibt einige wichtige Dinge zu beachten, wenn man die Länge eines Strings in PHP findet. Zum einen ist es wichtig zu wissen, dass die Funktionen `strlen()` und `mb_strlen()` die Zahl der Zeichen zählen, nicht die Zahl der Bytes. Das bedeutet, dass die Ausgabe je nach Zeichenkodierung unterschiedlich sein kann.

Ein weiteres wichtiges Konzept ist die Verwendung von Leerzeichen und Sonderzeichen in Strings. Diese werden ebenfalls als Zeichen gezählt und können die Länge des Strings beeinflussen. Es ist daher ratsam, Strings vorher zu bereinigen, bevor man ihre Länge bestimmt, um unerwartete Ergebnisse zu vermeiden.

## Siehe auch

- Offizielle PHP-Dokumentation zur `strlen()` Funktion: https://www.php.net/manual/en/function.strlen.php
- Offizielle PHP-Dokumentation zur `mb_strlen()` Funktion: https://www.php.net/manual/en/function.mb-strlen.php
- Stack Overflow Beitrag zur Längenbestimmung von Strings: https://stackoverflow.com/questions/473205/how-to-find-the-length-of-a-string-in-php