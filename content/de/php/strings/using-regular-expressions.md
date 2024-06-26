---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:47.116903-07:00
description: "Wie man es benutzt: PHP unterst\xFCtzt regul\xE4re Ausdr\xFCcke durch\
  \ die PCRE-Bibliothek (Perl Compatible Regular Expressions), die einen reichen Satz\
  \ von\u2026"
lastmod: '2024-03-13T22:44:53.962650-06:00'
model: gpt-4-0125-preview
summary: "PHP unterst\xFCtzt regul\xE4re Ausdr\xFCcke durch die PCRE-Bibliothek (Perl\
  \ Compatible Regular Expressions), die einen reichen Satz von Funktionen bietet."
title: "Regul\xE4re Ausdr\xFCcke verwenden"
weight: 11
---

## Wie man es benutzt:
PHP unterstützt reguläre Ausdrücke durch die PCRE-Bibliothek (Perl Compatible Regular Expressions), die einen reichen Satz von Funktionen bietet. So verwendet man sie:

### Ein Muster abgleichen:
Um zu überprüfen, ob ein Muster innerhalb einer Zeichenkette existiert, verwendet man `preg_match()`. Diese Funktion gibt 1 zurück, wenn das Muster in der Zeichenkette gefunden wurde, und 0, wenn nicht.

```php
if (preg_match("/\bweb\b/i", "PHP ist eine Skriptsprache für das Web")) {
    echo "Ein Treffer wurde gefunden.";
} else {
    echo "Es wurde kein Treffer gefunden.";
}
// Ausgabe: Ein Treffer wurde gefunden.
```

### Alle Treffer finden:
`preg_match_all()` wird verwendet, wenn man alle Vorkommen eines Musters in einer Zeichenkette finden muss.

```php
$text = "Katzen und Hunde";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Ausgabe: Array ( [0] => Katzen [1] => und [2] => Hunde )
```

### Text ersetzen:
Um Text zu ersetzen, der einem regulären Ausdruck entspricht, wird `preg_replace()` verwendet. Es ist unglaublich mächtig für die Formatierung und Bereinigung von Daten.

```php
$originalText = "15. April 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// Ausgabe: April1,2003
```

### Zeichenketten aufteilen:
Man kann eine Zeichenkette in ein Array aufteilen, indem man `preg_split()` verwendet und ein Muster für den Trenner angibt.

```php
$text = "PHP ist eine, extrem beliebte, Skriptsprache";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// Ausgabe: Array ( [0] => PHP ist [1] => eine extrem beliebte [2] => Skriptsprache )
```

Darüber hinaus können Rahmenwerke und Bibliotheken wie Symfony’s `Finder`-Komponente oder Laravels Sammlung von Hilfsfunktionen für komplexe regex-Muster und Aufgaben eine bequemere Abstraktionsebene bieten. Das Verständnis und die Nutzung der integrierten PCRE-Funktionen von PHP sind jedoch entscheidend für eine effiziente Textverarbeitung und Validierung direkt in PHP-Skripten.
