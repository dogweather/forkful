---
date: 2024-01-26 01:11:38.203546-07:00
description: "Code in Funktionen zu organisieren bedeutet, Ihren Code in wiederverwendbare\
  \ Bl\xF6cke mit definierten Zwecken zu unterteilen. Wir tun dies, um Ordnung zu\u2026"
lastmod: '2024-03-13T22:44:53.977960-06:00'
model: gpt-4-1106-preview
summary: "Code in Funktionen zu organisieren bedeutet, Ihren Code in wiederverwendbare\
  \ Bl\xF6cke mit definierten Zwecken zu unterteilen."
title: Code in Funktionen organisieren
weight: 18
---

## Was & Warum?
Code in Funktionen zu organisieren bedeutet, Ihren Code in wiederverwendbare Blöcke mit definierten Zwecken zu unterteilen. Wir tun dies, um Ordnung zu bewahren, Redundanzen zu vermeiden und das Debugging zum Kinderspiel zu machen.

## Wie geht das:
Stellen Sie sich vor, wir haben wiederholenden Code zum Begrüßen von Nutzern. Stattdessen werden wir ihn in einer Funktion wie `greet_user` einpacken:

```php
function greet_user($name) {
    return "Hallo, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Ausgabe:
```
Hallo, Alice!
Hallo, Bob!
```

Jetzt haben Sie ein praktisches Werkzeug, das Sie jederzeit verwenden können, ohne jedes Mal dieselben Zeilen Code neu schreiben zu müssen, wenn Sie Hallo sagen möchten.

## Vertiefung
Funktionen gibt es in der Programmierung seit den frühen Tagen von FORTRAN in den 50er Jahren. Sie sind ein Grundpfeiler der strukturierten Programmierung und stehen ganz im Zeichen von Modularität und Isolation. Alternativen? Nun, man könnte sich der objektorientierten Programmierung zuwenden und über Klassen und Methoden sprechen, die Funktionen in einem schickeren Gewand sind. Was PHP betrifft, so umfassen die Implementierungsdetails das Festlegen von Standardwerten für Parameter, das Vorschreiben von Typen für Eingaben und die Möglichkeit, mehrere Werte zurückzugeben, indem man ein Array verwendet oder, ab PHP 7.1, eine Liste.

Hier ist eine moderne Variante mit Typdeklaration und Standardwerten:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 führte auch Pfeilfunktionen ein, die dabei helfen, knappe Ein-Zeilen-Funktionen zu schreiben, wie sie häufig bei Array-Operationen verwendet werden:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Ausgabe:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Siehe auch
- [PHP-Handbuch zu Funktionen](https://www.php.net/manual/de/functions.user-defined.php)
- [PHP: The Right Way - Funktionen](https://phptherightway.com/#functions)
- [Mehr über PHP 7.4 Pfeilfunktionen lernen](https://stitcher.io/blog/short-closures-in-php)
