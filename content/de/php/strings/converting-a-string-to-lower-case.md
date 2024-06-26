---
date: 2024-01-20 17:39:11.716170-07:00
description: "So geht's: Das Umwandeln einer Zeichenkette (String) in Kleinbuchstaben\
  \ gibt es seit den ersten Versionen von PHP. Historisch betrachtet war `strtolower`\u2026"
lastmod: '2024-04-05T22:51:08.515956-06:00'
model: gpt-4-1106-preview
summary: Das Umwandeln einer Zeichenkette (String) in Kleinbuchstaben gibt es seit
  den ersten Versionen von PHP.
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## So geht's:
```PHP
<?php
$original_string = "PHP macht Spaß!";
$lowercase_string = strtolower($original_string);

echo $lowercase_string;  // Ausgabe: php macht spaß!
?>
```

Beispiel mit Umlauten:
```PHP
<?php
$original_string = "BärÖlÜber den Äther";
$lowercase_string = mb_strtolower($original_string, 'UTF-8');

echo $lowercase_string;  // Ausgabe: bärölüber den äther
?>
```

## Tiefgang:
Das Umwandeln einer Zeichenkette (String) in Kleinbuchstaben gibt es seit den ersten Versionen von PHP. Historisch betrachtet war `strtolower` zunächst nur für ASCII-Zeichen geeignet. Mit dem Aufkommen von Mehrsprachigkeit und Zeichenkodierungen wie UTF-8 entstand das `mb_strtolower`, Teil des "Multibyte String" Erweiterungspakets, um dieses Manko zu überwinden.

Alternativen zu `strtolower` und `mb_strtolower` beinhalten `strtoupper` (für Großbuchstaben), sowie verschiedene Anpassungen über `mb_convert_case` oder benutzerdefinierte Funktionen mit `strtr` oder regulären Ausdrücken.

Ein wichtiger Aspekt in der Implementierung ist die Beachtung der richtigen Zeichenkodierung. Während `strtolower` ohne Zweite-Argument-Eingabe funktioniert, benötigt `mb_strtolower` diese manchmal, um die Kodierung spezifisch anzugeben, welche bei Umlauten und anderen nicht-ASCII Zeichen wichtig ist.

## Siehe auch:
- PHP Dokumentation für `strtolower`: https://www.php.net/manual/de/function.strtolower.php
- PHP Dokumentation für `mb_strtolower`: https://www.php.net/manual/de/function.mb-strtolower.php
- PHP Dokumentation für Multibyte-String-Erweiterung: https://www.php.net/manual/de/book.mbstring.php

Denk daran: praktische Erfahrung gewinnst du durch Ausprobieren und Anwenden dieser Funktionen in deinen eigenen Skripten. Frohes Codieren!
