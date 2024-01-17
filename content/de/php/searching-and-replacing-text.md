---
title:                "Suchen und Ersetzen von Text"
html_title:           "PHP: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Suchen und Ersetzen von Text ist ein gängiges Verfahren in der Programmierung, bei dem ein bestimmtes Zeichen oder eine bestimmte Sequenz innerhalb eines Textes gefunden und durch einen anderen Text ersetzt wird. Programmierer verwenden diese Methode, um schnell und effizient Änderungen in ihrem Code durchzuführen, anstatt den Text manuell zu bearbeiten. Dies spart Zeit und verringert die Möglichkeit von Tippfehlern.

## So geht's:
Um in PHP Text zu suchen und zu ersetzen, können wir die Funktion `str_replace()` verwenden. Hier ist ein Beispiel, um alle Vorkommen des Wortes "Hallo" im String "Hallo Welt" durch "Guten Tag" zu ersetzen:

```PHP
<?php
$string = "Hallo Welt";
$new_string = str_replace("Hallo", "Guten Tag", $string);
echo $new_string; // Ausgabe: Guten Tag Welt
?>
```

Wir können auch mithilfe von regulären Ausdrücken (Regex) nach bestimmten Mustern suchen und diese ersetzen. In diesem Beispiel ersetzen wir alle Zahlen im String "Hallo 123" durch das Wort "Welt":

```PHP
<?php
$string = "Hallo 123";
$new_string = preg_replace("/\d+/", "Welt", $string);
echo $new_string; // Ausgabe: Hallo Welt
?>
```

## Tiefere Einblicke:
Das Suchen und Ersetzen von Text hat eine lange Geschichte und wird in vielen Programmiersprachen verwendet. In PHP gibt es neben der `str_replace()`- und `preg_replace()`-Funktion auch noch die `substr_replace()`-Funktion, die es uns ermöglicht, nur einen Teil des Strings zu ersetzen. Alternativ können wir auch die `strtr()`-Funktion verwenden, um Zeichen für Zeichen zu ersetzen.

Es gibt auch alternative Wege, um Text zu suchen und zu ersetzen, wie zum Beispiel in der Datenbank oder mit Hilfe von Shell-Befehlen. Es ist wichtig zu wissen, welche Methode für welchen Zweck am besten geeignet ist.

## Siehe auch:
- [PHP-Dokumentation für str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP-Dokumentation für preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP-Dokumentation für substr_replace()](https://www.php.net/manual/en/function.substr-replace.php)
- [PHP-Dokumentation für strtr()](https://www.php.net/manual/en/function.strtr.php)