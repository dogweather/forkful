---
title:    "PHP: Entfernen von Zeichen mit übereinstimmendem Muster"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Oftmals müssen wir in der Programmierung Zeichen löschen, die einem bestimmten Muster entsprechen. Dies kann zum Beispiel bei der Validierung von Benutzereingaben oder der Bereinigung von Datenbanken nützlich sein. In diesem Blog-Beitrag werde ich erklären, wie man in PHP Zeichen anhand eines bestimmten Musters löschen kann.

## Wie es geht

Um Zeichen in PHP zu löschen, die einem bestimmten Muster entsprechen, können wir die Funktion `preg_replace()` verwenden. Diese Funktion erwartet drei Parameter: das Muster, nach dem gesucht werden soll, der Text, in dem gesucht werden soll, und der Text, der anstelle des gefundenen Musters eingefügt werden soll. 

In dem folgenden Beispiel werden alle Zahlen aus einem String gelöscht und der neue String ausgegeben:

```PHP
<?php
$string = "123abc45def";
echo preg_replace("/[0-9]/", "", $string);
// Output: abcdef
```

Das Muster `"/[0-9]/"` sucht nach allen Zahlen zwischen 0 und 9 und die leeren Anführungszeichen `""` bedeuten, dass diese durch nichts ersetzt werden sollen.

## Tiefergehende Erklärung

Die `preg_replace()` Funktion basiert auf regulären Ausdrücken, also Muster, die verwendet werden, um Strings auf bestimmte Zeichenfolgen zu überprüfen. Diese Muster werden in Schrägstrichen `//` eingeschlossen. Im obigen Beispiel haben wir das Muster `"/[0-9]/"` verwendet, um nach Zahlen zu suchen. Hier einige weitere Beispiele für reguläre Ausdrücke:

- `"/[a-z]/"` sucht nach Kleinbuchstaben
- `"/[A-Z]/"` sucht nach Großbuchstaben
- `"/[a-zA-Z0-9]/"` sucht nach Buchstaben und Zahlen

Reguläre Ausdrücke können aber noch viel komplexer werden und beinhalten eine Vielzahl von Metazeichen, die noch weiter angepasst werden können. Für tiefergehende Informationen zu regulären Ausdrücken empfehle ich die offizielle PHP-Dokumentation.

## Siehe auch

- Offizielle PHP-Dokumentation zu regulären Ausdrücken: https://www.php.net/manual/de/pcre.pattern.php
- Tutorial zu regulären Ausdrücken in PHP (auf Deutsch): https://www.php-einfach.de/mysql-tutorial/regex-regular-expressions/