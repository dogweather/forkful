---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
date:                  2024-01-20T17:43:11.855130-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/deleting-characters-matching-a-pattern.md"
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
