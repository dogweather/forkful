---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:56.366719-07:00
description: "Das Gro\xDFschreiben eines Strings beinhaltet die Modifikation des ersten\
  \ Zeichens eines gegebenen Textes in Gro\xDFbuchstaben, um sicherzustellen, dass\
  \ S\xE4tze,\u2026"
lastmod: '2024-03-13T22:44:53.955739-06:00'
model: gpt-4-0125-preview
summary: "Das Gro\xDFschreiben eines Strings beinhaltet die Modifikation des ersten\
  \ Zeichens eines gegebenen Textes in Gro\xDFbuchstaben, um sicherzustellen, dass\
  \ S\xE4tze, Titel oder Eigennamen korrekt in einem Datensatz beginnen."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie:
PHP unterstützt nativ verschiedene Funktionen zum Großschreiben von Strings, die jeweils unterschiedlichen Zwecken dienen. Hier ist, wie Sie sie verwenden können:

### Großschreiben des ersten Buchstabens eines Strings:
```php
$string = "hallo, welt!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Gibt aus: Hallo, welt!
```

### Großschreiben des ersten Buchstabens jedes Wortes:
```php
$string = "hallo, welt!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Gibt aus: Hallo, Welt!
```

### Umwandeln des gesamten Strings in Großbuchstaben:
```php
$string = "hallo, welt!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Gibt aus: HALLO, WELT!
```

Für Szenarien, die mehr Anpassung oder Drittlösungen erfordern, können Bibliotheken wie `mbstring` (für Multibyte-Strings) verwendet werden, insbesondere, wenn es um Internationalisierung geht, bei der Zeichen über den grundlegenden ASCII-Satz hinausgehen können.

### Verwendung von mbstring zum Großschreiben von UTF-8-Strings:
Stellen Sie sicher, dass die `mbstring`-Erweiterung in Ihrer PHP-Konfiguration aktiviert ist, dann:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Gibt aus: Élégant
```

Dieser Ansatz hilft, Strings, die Nicht-ASCII-Zeichen enthalten, genau zu großzuschreiben und den Feinheiten verschiedener Sprachen gerecht zu werden.
