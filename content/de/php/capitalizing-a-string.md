---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings bedeutet, alle Buchstaben in einem Text zu Großbuchstaben zu konvertieren; praktisch für Titel oder wenn Konsistenz erforderlich ist. Entwickler nutzen das, um Lesbarkeit zu verbessern oder um Daten zu normalisieren, bevor sie verglichen oder gespeichert werden.

## How to:
Hier ein einfaches Beispiel, wie du einen String in PHP großschreiben kannst:

```php
<?php
$originalString = "das ist ein test";
$capitalizedString = strtoupper($originalString);
echo $capitalizedString; // Ausgabe: DAS IST EIN TEST
?>
```

Und so verwendest du `mb_strtoupper()` für Strings mit Multibyte-Zeichen (wie Umlaute oder andere nicht-ASCII-Zeichen):

```php
<?php
$originalString = "füße überflüssig";
$capitalizedString = mb_strtoupper($originalString);
echo $capitalizedString; // Ausgabe: FÜßE ÜBERFLÜSSIG
?>
```

## Deep Dive:
Das Großschreiben von Strings ist seit den Anfängen von PHP ein grundlegendes Feature, das durch Funktionen wie `strtoupper()` ermöglicht wird. PHP bietet auch `mb_strtoupper()` für die Arbeit mit Multibyte-Zeichen, die in verschiedenen Sprachen und Kodierungen wichtig sind (z.B. UTF-8). Während `strtoupper()` nur für Single-Byte-Zeichen (ASCII) geeignet ist, ist `mb_strtoupper()` flexibel genug, um beispielsweise deutsche Umlaute korrekt zu verarbeiten.

Alternativ zur vollständigen Großschreibung gibt es auch `ucfirst()` zum Großschreiben des ersten Buchstabens und `ucwords()`, um den ersten Buchstaben jedes Wortes in einem String zu großzuschreiben. Die Wahl der Funktion hängt vom gewünschten Ergebnis ab.

Die Implementierungsdetails von `strtoupper()` und `mb_strtoupper()` unterscheiden sich in der Art, wie sie mit Zeichensätzen umgehen. `strtoupper()` betrachtet den String als Sammlung von Einzelbyte-Zeichen, während `mb_strtoupper()` eine Multibyte-freundliche Betrachtung hat und auf die angegebene Zeichenkodierung in `mb_internal_encoding()` oder einen optionalen Parameter achtet.

## See Also:
Die offizielle PHP-Dokumentation ist deine Anlaufstelle, um tiefer in die Materie einzutauchen und weitere Funktionen rund um die String-Behandlung in PHP zu entdecken:

- PHP `strtoupper()` Funktion: [php.net/manual/de/function.strtoupper.php](https://www.php.net/manual/de/function.strtoupper.php)
- PHP `mb_strtoupper()` Funktion: [php.net/manual/de/function.mb-strtoupper.php](https://www.php.net/manual/de/function.mb-strtoupper.php)
- PHP String-Funktionen: [php.net/manual/de/ref.strings.php](https://www.php.net/manual/de/ref.strings.php)
