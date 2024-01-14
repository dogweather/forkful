---
title:                "PHP: Großschreibung eines String"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum
Jeder, der schon einmal PHP programmiert hat, kennt das Problem: Man benötigt eine Zeichenfolge (String), die komplett in Großbuchstaben geschrieben ist. Egal ob für Überschriften oder für Datenvergleiche - eine korrekte Großschreibung ist oft unerlässlich. Aber warum ist das so wichtig?

## How To
Es gibt verschiedene Möglichkeiten, einen String in PHP zu groß zu schreiben. Die einfachste und schnellste Methode ist die Verwendung der Funktion `strtoupper()`. Diese Funktion wandelt alle Buchstaben des Strings in Großbuchstaben um. Ein Beispiel:

```PHP
<?php
$string = "Hallo Welt";
echo strtoupper($string);
// Ausgabe: HALLO WELT
?>
```

Eine andere Möglichkeit ist die Verwendung der Funktion `mb_strtoupper()`, welche auch für mehrzeilige Strings geeignet ist und die korrekte Großschreibung für alle Sprachen unterstützt.

Um aber noch einen Schritt weiterzugehen und auch Sonderzeichen und Umlaute richtig zu behandeln, kann man die Funktion `mb_convert_case()` nutzen. Hier kann man angeben, ob man den String komplett in Großbuchstaben haben möchte oder auch nur den ersten Buchstaben jedes Wortes.

```PHP
<?php
$string = "über den Wolken";
$string = mb_convert_case($string, MB_CASE_UPPER, "UTF-8");
echo $string;
// Ausgabe: ÜBER DEN WOLKEN
?>
```

## Deep Dive
Wenn man tiefer in das Thema eintauchen möchte, gibt es noch weitere interessante Aspekte zu beachten. Zum Beispiel kann man mit der Funktion `mb_strtoupper()` auch den Text in verschiedenen Kodierungen umwandeln, wie zum Beispiel in UTF-8 oder ISO-8859-1. 

Außerdem gibt es auch verschiedene Einstellungsmöglichkeiten für das Verhalten bei Sonderzeichen und Umlauten. Diese können mithilfe der Funktion `mb_internal_encoding()` festgelegt werden.

Es ist auch wichtig zu wissen, dass die Funktionen `strtoupper()` und `mb_strtoupper()` nur mit lateinischen Zeichen funktionieren. Für andere Sprachen gibt es spezielle Funktionen, wie zum Beispiel `mb_strtoupper()` für kyrillische Zeichen.

## Siehe auch
- [PHP Manual - strtoupper()](https://www.php.net/manual/de/function.strtoupper.php)
- [PHP Manual - mb_strtoupper()](https://www.php.net/manual/de/function.mb-strtoupper.php) 
- [PHP Manual - mb_convert_case()](https://www.php.net/manual/de/function.mb-convert-case.php)