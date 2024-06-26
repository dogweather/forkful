---
date: 2024-01-20 17:58:27.928375-07:00
description: "Wie geht das? Um Text in PHP zu suchen und zu ersetzen, verwenden wir\
  \ h\xE4ufig `str_replace()` oder `preg_replace()` f\xFCr komplexere, muster-basierte\u2026"
lastmod: '2024-03-13T22:44:53.957634-06:00'
model: gpt-4-1106-preview
summary: "Um Text in PHP zu suchen und zu ersetzen, verwenden wir h\xE4ufig `str_replace()`\
  \ oder `preg_replace()` f\xFCr komplexere, muster-basierte Operationen."
title: Suchen und Ersetzen von Text
weight: 10
---

## Wie geht das?
Um Text in PHP zu suchen und zu ersetzen, verwenden wir häufig `str_replace()` oder `preg_replace()` für komplexere, muster-basierte Operationen. Hier ein schneller Durchlauf:

```PHP
<?php

$text = "Der Fuchs springt über den faulen Hund";

// Einfaches Suchen und Ersetzen
$neuerText = str_replace("Fuchs", "Hase", $text);
echo $neuerText; // Der Hase springt über den faulen Hund

// Muster-basiertes Suchen und Ersetzen mit regulären Ausdrücken
$neuerText = preg_replace("/über den (.*) Hund/", "um den schlauen Fuchs", $text);
echo $neuerText; // Der Fuchs springt um den schlauen Fuchs

?>
```

## Tiefgang
Die Funktionen `str_replace()` und `preg_replace()` sind nur die Spitze des Eisbergs. Vor langer Zeit mussten Entwickler einiges an Arbeit aufwenden, um ähnliche Resultate zu erzielen, meist mit langen Schleifen und einer Menge String-Manipulation.

Alternativ zu `str_replace()` haben wir `str_ireplace()`, das ohne Beachtung von Groß- und Kleinschreibung ersetzt – praktisch!

`preg_replace()` nutzt dagegen reguläre Ausdrücke, die mächtige Mustererkennungen ermöglichen. Es basiert auf der PCRE (Perl Compatible Regular Expressions) Library und kann ziemlich komplex werden. Timing und Speicherauslastung sind hier die häufigsten Stolpersteine, besonders bei riesigen Textmengen.

## Siehe auch
- Die offizielle PHP Dokumentation für `str_replace()`: https://www.php.net/manual/de/function.str-replace.php
- PHP.net zu `preg_replace()`: https://www.php.net/manual/de/function.preg-replace.php
- Ein Tutorial zu regulären Ausdrücken in PHP: https://www.phptutorial.net/php-tutorial/php-regular-expressions/
