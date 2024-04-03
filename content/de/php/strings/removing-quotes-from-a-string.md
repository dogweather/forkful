---
date: 2024-01-26 03:40:38.035723-07:00
description: 'Wie geht das: Hier ist ein einfaches Beispiel unter Verwendung der integrierten
  Funktionen von PHP.'
lastmod: '2024-03-13T22:44:53.960322-06:00'
model: gpt-4-0125-preview
summary: Hier ist ein einfaches Beispiel unter Verwendung der integrierten Funktionen
  von PHP.
title: "Anf\xFChrungszeichen aus einem String entfernen"
weight: 9
---

## Wie geht das:
Hier ist ein einfaches Beispiel unter Verwendung der integrierten Funktionen von PHP:

```php
$quotedString = "'Hallo,' sagte sie, \"Es ist ein schöner Tag!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Gibt aus: Hallo, sagte sie, Es ist ein schöner Tag!
```

Einfach, oder? Diese `str_replace()` Funktion nimmt ein Array von Zeichen, die aus dem String entfernt werden sollen, einschließlich sowohl einfacher als auch doppelter Anführungszeichen.

## Tiefer eintauchen
Zurück in den frühen Tagen von PHP mussten Entwickler besonders vorsichtig mit Anführungszeichen in Strings sein, vor allem beim Einfügen von Daten in eine Datenbank. Unzureichend gehandhabte Anführungszeichen könnten zu SQL-Injection-Angriffen führen. Dann kam die Funktion der magischen Anführungszeichen, eine Funktion, die Eingabedaten automatisch escaped. Sie wurde als veraltet erklärt und schließlich entfernt, weil sie schlechte Programmierpraktiken und Sicherheitsprobleme förderte.

Heutzutage verwenden wir Funktionen wie `str_replace()` oder reguläre Ausdrücke mit `preg_replace()` für komplexere Muster. Hier ist ein Beispiel mit regulären Ausdrücken:

```php
$quotedString = "'Hallo,' sagte sie, \"Es ist ein schöner Tag!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Für JSON-Daten könnten Sie `json_encode()` mit Optionen wie `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` verwenden, um zusätzliche Rückstriche in Ihren Anführungszeichen zu vermeiden.

Bei der Implementierung, sollten Sie Sonderfälle berücksichtigen. Was ist, wenn Ihr String bestimmte Anführungszeichen enthalten soll, wie Dialoge in einer Geschichte oder Zoll in Maßangaben? Der Kontext ist wichtig, also passen Sie das Entfernen von Anführungszeichen der beabsichtigten Verwendung der Daten an.

## Siehe auch
- [PHP: str_replace](https://www.php.net/manual/de/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/de/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/de/function.json-encode.php)
- [OWASP: Verhinderung von SQL-Injection](https://owasp.org/www-community/attacks/SQL_Injection)
