---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen von Text ist eine grundlegende Funktion, die sich auf die Identifizierung und Änderung bestimmter Sequenzen von Zeichen in einer Zeichenkette bezieht. Sie ist besonders nützlich beim Automatisieren von Code-Revisionen und Manipulationen von Benutzereingaben. 

## So geht's:

PHP bietet eine Reihe von Funktionen zum Suchen und Ersetzen von Text. Eine der am häufigsten verwendeten ist `str_replace()`. Hier ist ein einfaches Beispiel:

```PHP
<?php
$text = "Hallo, Welt!";
$text = str_replace("Welt", "Nutzer", $text);
echo $text; // Ausgabe: "Hallo, Nutzer!"
?>
```

Mit dieser Funktion kann man auch Arrays verwenden, um mehrere Begriffe gleichzeitig zu suchen und zu ersetzen:

```PHP
<?php
$text = "Ich liebe Äpfel und Bananen.";
$find = array("Äpfel", "Bananen");
$replace = array("Birnen", "Erdbeeren");
$text = str_replace($find, $replace, $text);
echo $text; // Ausgabe: "Ich liebe Birnen und Erdbeeren."
?>
```

## Vertiefung:

Die PHP-Funktion `str_replace()` ist seit Version 4.0.0 verfügbar und hat sich im Laufe der Zeit weiterentwickelt. Alternativen zu `str_replace()` sind `preg_replace()` für reguläre Ausdrücke und `str_ireplace()` für die Groß- und Kleinschreibung.

Das Verständnis des genauen Prozesses, wie `str_replace()` arbeitet, kann Ihnen helfen, effektiver zu programmieren. Die Funktion durchläuft die Zeichenkette von Anfang bis Ende und ersetzt jedes Vorkommen des Suchstrings durch den Ersetzungsstring, wobei sie immer den ersten Vorkommen des Strings ersetzt, den sie findet.

## Siehe auch:

1. PHP-Handbuch zu `str_replace()`: [https://www.php.net/manual/de/function.str-replace.php](https://www.php.net/manual/de/function.str-replace.php)
3. PHP-Dokumentation zu `preg_replace()`: [https://www.php.net/manual/de/function.preg-replace.php](https://www.php.net/manual/de/function.preg-replace.php)
4. PHP-Dokumentation zu `str_ireplace()`: [https://www.php.net/manual/de/function.str-ireplace.php](https://www.php.net/manual/de/function.str-ireplace.php)