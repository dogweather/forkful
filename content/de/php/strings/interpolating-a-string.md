---
title:                "Zeichenketten interpolieren"
date:                  2024-01-20T17:51:22.992786-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ermöglicht es uns, Variablen nahtlos in Zeichenketten einzufügen. Programmierer nutzen das, um dynamisch Inhalte zu generieren, ohne ständig Strings zusammenzufügen.

## How to:
Interpolieren in PHP ist ein Kinderspiel. Fügen Sie einfach eine Variable in einen doppelten Anführungszeichen-String oder Heredoc ein:

```php
$name = "Welt";
echo "Hallo, $name!"; // Ausgabe: Hallo, Welt!
```

Nutzen Sie geschweifte Klammern, wenn es komplexer wird:

```php
$item = "Apfel";
echo "Ich esse gerne {$item}s"; // Ausgabe: Ich esse gerne Äpfel
```

## Deep Dive
String-Interpolation gibt's in PHP seit Urzeiten – ein Grund, warum die Sprache so schnell für dynamische Inhalte eingesetzt wurde. Es gibt Alternativen wie `sprintf` oder die Konkatenation mit `.`:

```php
echo 'Hallo, ' . $name . '!'; // Alternativ, aber umständlicher
```

Im Inneren verarbeitet der PHP-Parser interpolierte Strings als Teile, wodurch sie etwas performanter als Konkatenation sind. Vor PHP 8.0 macht es kaum einen Unterschied, aber PHP 8.x hat Optimierungen, die Interpolation effizienter machen.

## Siehe auch
- Die PHP-Dokumentation zur [String-Interpolation](https://www.php.net/manual/de/language.types.string.php#language.types.string.parsing)
- [Heredoc-Syntax](https://www.php.net/manual/de/language.types.string.php#language.types.string.syntax.heredoc) in der PHP-Doku
- php.net über [sprintf](https://www.php.net/manual/de/function.sprintf.php) und warum es manchmal besser ist
