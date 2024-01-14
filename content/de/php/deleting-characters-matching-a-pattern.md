---
title:                "PHP: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann nützlich sein, wenn man in einer Zeichenkette nach unerwünschten Elementen sucht oder diese durch andere Zeichen ersetzen möchte.

## Wie geht's?

Um Zeichen in einer Zeichenkette zu löschen, die einem bestimmten Muster entsprechen, kann man die PHP-Funktion `preg_replace()` verwenden. Hier ein Beispiel:

```PHP
$string = "Dies ist ein Beispieltext123!";

// Lösche alle Zahlen aus der Zeichenkette
$new_string = preg_replace("/[0-9]/", "", $string);

// Ausgabe: Dies ist ein Beispieltext!
echo $new_string;
```

Das Muster "/[0-9]/" gibt an, dass alle Zahlen in der Zeichenkette gelöscht werden sollen. Mit dem leeren String als zweitem Argument wird das gelöschte Muster durch nichts ersetzt, somit wird es einfach entfernt. In diesem Beispiel wird also jede Zahl in der Zeichenkette durch nichts ersetzt.

Man kann auch mehrere Muster angeben und alle gleichzeitig löschen:

```PHP
$string = "Dies ist ein Beispieltext123!";

// Lösche alle Zahlen und Sonderzeichen aus der Zeichenkette
$new_string = preg_replace("/[0-9!#]/", "", $string);

// Ausgabe: Dies ist ein Beispieltext
echo $new_string;
```

Hier wird angegeben, dass alle Zahlen, Ausrufezeichen und Hashtags gelöscht werden sollen.

## Tiefer Einblick

Die `preg_replace()`-Funktion verwendet reguläre Ausdrücke, auch bekannt als Regex, um Muster in einer Zeichenkette zu suchen und zu ersetzen. Reguläre Ausdrücke sind sehr flexibel und mächtig, aber auch komplex zu verstehen.

Das Muster "/[0-9]/" bedeutet zum Beispiel, dass jede Ziffer von 0 bis 9 in der Zeichenkette gelöscht wird. Man kann auch angeben, wie oft das Muster vorkommen soll, zum Beispiel "/[0-9]{2}/" würde nur zwei Ziffern zusammenhängend löschen.

Für eine detaillierte Anleitung zu regulären Ausdrücken empfehle ich den Artikel "Eine Einführung in reguläre Ausdrücke in PHP" von [PHP Einfach](https://www.php-einfach.de/experte/php-codebeispiele/regex-regulaere-ausdruecke-in-php/).

## Siehe auch

- [PHP Einfach - Eine Einführung in reguläre Ausdrücke in PHP](https://www.php-einfach.de/experte/php-codebeispiele/regex-regulaere-ausdruecke-in-php/)
- [PHP-Handbuch - preg_replace()](https://www.php.net/manual/de/function.preg-replace.php)
- [RegExr - Interaktives Tool zum Testen von regulären Ausdrücken](https://regexr.com/)