---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# PHP Tutorials: Zeichenkettenlänge finden

## Was & Warum?

Stränge (Strings), sind Folgen von Zeichen. Das Finden ihrer Länge ist das Zählen der Zeichen in einer Zeichenkette. Programmierer machen das oft, um die Anforderungen bestimmter Funktionen zu erfüllen, z. B. das Validieren der Länge von Benutzereingaben.

## So geht’s:

In PHP nutzt man die `strlen()` Funktion, um die Länge einer Zeichenkette zu ermitteln. Sehen wir uns das Beispiel unten an:

```PHP
<?php
    $str = "Hallo Welt!";
    echo strlen($str);
?>
```

Die Ausgabe wäre `12`, da die Zeichenkette "Hallo Welt!" zwölf Zeichen lang ist (einschließlich Leerzeichen).

## Vertiefung:

Historisch betrachtet hat sich die String-Längenbestimmung daraus entwickelt, dass in frühen Computersystemen die Zeichenkettenlänge oft durch eine spezielle Endemarke gekennzeichnet wurde. Auch heute ist es in einigen anderen Programmiersprachen immer noch üblich.

Eine mögliche Alternative zur `strlen()` Funktion ist die `mb_strlen()` Funktion. Sie ist hilfreich, wenn mit Multibyte-Zeichensätzen (z. B. UTF-8) gearbeitet wird.

```PHP
<?php
    $str = "Hällo Wëlt!";
    echo mb_strlen($str, "UTF-8");
?>
```
Die Ausgabe ist wieder `12`, obwohl einige der Zeichen mehrere Bytes groß sind.

Die `strlen()` Funktion zählt einfach die Bytes, während `mb_strlen()` tatsächlich die Zeichen zählt.

## Weiterführende Informationen:

Weitere Informationen zum Arbeiten mit Zeichenketten in PHP finden Sie in der PHP-Dokumentation:
- PHP strlen - https://www.php.net/manual/de/function.strlen.php
- PHP mb_strlen - https://www.php.net/manual/de/function.mb-strlen.php