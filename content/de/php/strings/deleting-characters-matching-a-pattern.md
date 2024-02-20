---
date: 2024-01-20 17:43:11.855130-07:00
description: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet,\
  \ bestimmte Teile aus einem String zu entfernen, die einem vorgegebenen Muster folgen.\u2026"
lastmod: 2024-02-19 22:05:12.885326
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte\
  \ Teile aus einem String zu entfernen, die einem vorgegebenen Muster folgen.\u2026"
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte Teile aus einem String zu entfernen, die einem vorgegebenen Muster folgen. Programmierer machen das, um Eingaben zu säubern, Daten zu formatieren oder unnötige Informationen zu entfernen.

## Anleitung:
Hier ein Beispiel, wie man Zeichen in PHP löscht:

```php
<?php
$text = "Hallo Welt! 123";
$pattern = '/[0-9]+/';

$cleanedText = preg_replace($pattern, '', $text);

echo $cleanedText; // Gibt aus: Hallo Welt!
?>
```

Dieser Code benutzt `preg_replace()`, um alle Zahlen aus dem `$text` zu entfernen. Nach der Ersetzung enthält `$cleanedText` den String ohne Ziffern.

## Vertiefung:
Zum Löschen von Zeichen nach einem Muster wird meistens reguläre Ausdrücke (Regex) verwendet, die es seit den 1950er-Jahren gibt und in der UNIX-Welt populär wurden. PHP implementiert Regex mithilfe der PCRE (Perl Compatible Regular Expressions) Library. Alternativen zur `preg_replace()`-Funktion sind `str_replace()` und `str_ireplace()`, die allerdings keine Mustererkennung bieten, sondern nur einfache Zeichenkettenersetzung ermöglichen. Ein interessanter Aspekt bei der Implementierung von `preg_replace()` ist, dass die Funktion auch Rückrufe (Callbacks) mittels `preg_replace_callback()` unterstützt, was komplexere Ersetzungsmuster ermöglicht.

## Siehe Auch:
- Die offizielle PHP-Dokumentation zu `preg_replace()`: https://www.php.net/manual/de/function.preg-replace.php
- Ein Tutorial zu regulären Ausdrücken in PHP: https://www.phptutorial.net/php-tutorial/php-regular-expressions/
- PCRE-Dokumentation für tieferes Verständnis: https://www.pcre.org/current/doc/html/
