---
date: 2024-01-20 17:48:05.160031-07:00
description: "So geht\u2019s: Mit `strlen()` bekommst du schnell und unkompliziert\
  \ die L\xE4nge eines Strings. Leicht, oder?."
lastmod: '2024-04-05T21:53:55.848254-06:00'
model: gpt-4-1106-preview
summary: "Mit `strlen()` bekommst du schnell und unkompliziert die L\xE4nge eines\
  \ Strings."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## So geht’s:
```PHP
<?php
$text = "Hallo Welt!";
$laenge = strlen($text);
echo $laenge; // Gibt 11 aus
?>
```
Mit `strlen()` bekommst du schnell und unkompliziert die Länge eines Strings. Leicht, oder?

```PHP
<?php
$text = "Ein längerer Satz mit mehreren Zeichen.";
echo strlen($text); // Gibt 36 aus
?>
```
Jeder Buchstabe, jedes Leerzeichen, jedes Zeichen zählt.

## Deep Dive
Früher, in den Anfängen von PHP, war `strlen()` die Hauptfunktion, um die Länge eines Strings zu messen. Jetzt ist es immer noch die Standardmethode, jedoch gibt es auch die `mb_strlen()` Funktion aus dem `mbstring`-Modul. Sie unterstützt Mehrbyte-Zeichensätze, wie UTF-8. Warum ist das wichtig? Nun, in Zeiten der Globalisierung muss Code auch internationale Zeichen problemlos bewältigen.

`strlen()` zählt einfach die Bytes, das ist in ASCII okay, aber bei UTF-8 kannst du falsche Werte erhalten, da einige Zeichen mehr als ein Byte groß sind. `mb_strlen()` betrachtet die tatsächliche Zeichenanzahl.

## Siehe Auch
- PHP Manual on `strlen()`: https://www.php.net/manual/function.strlen.php
- PHP Manual on `mb_strlen()`: https://www.php.net/manual/function.mb-strlen.php
- Wikipedia on Character encoding: https://en.wikipedia.org/wiki/Character_encoding

Lerne mehr über Zeichencodierung, um zu verstehen, wie und warum manche Zeichen mehr Platz beanspruchen. Und vergiss nicht: Praxis macht den Meister – probier es einfach aus!
