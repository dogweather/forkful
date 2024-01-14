---
title:                "PHP: Verwendung von regulären Ausdrücken"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum
Die Verwendung von regulären Ausdrücken kann die Arbeit von Programmierern erheblich erleichtern. Sie ermöglichen es, komplexe Suchmuster in Texten zu definieren und somit effizienter und präziser zu arbeiten.

## How To
Reguläre Ausdrücke in PHP werden mit Hilfe der `preg_match()` Funktion verwendet. Beispiel:

```PHP
$text = "Hallo, mein Name ist Max";
if (preg_match("/Max/", $text)) {
   echo "Der Text enthält den Namen Max";
}
```

Dieser Code durchsucht den gegebenen Text nach dem Suchmuster "Max" und gibt eine Nachricht aus, wenn dieser gefunden wurde. Die Ausgabe lautet: "Der Text enthält den Namen Max".

Reguläre Ausdrücke ermöglichen es auch, Variablen in das Suchmuster einzubinden, um so noch präzisere Suchen durchzuführen. Beispiel:

```PHP
$text = "Ich habe 3 Hunde und 2 Katzen";
if (preg_match("/(\d+) Hunde und (\d+) Katzen/", $text, $matches)) {
   echo "In dem Text wurden ".$matches[1]." Hunde und ".$matches[2]." Katzen gefunden";
}
```

Dieser Code sucht nach der Anzahl der Hunde und Katzen im Text und gibt diese aus. Die Ausgabe lautet: "In dem Text wurden 3 Hunde und 2 Katzen gefunden".

## Deep Dive
Reguläre Ausdrücke können auch eine Vielzahl von Wildcards und Modifikatoren enthalten, um noch komplexere Suchmuster zu definieren. Einige Beispiele sind:

- `.` als Wildcard für ein beliebiges Zeichen
- `*` für null oder mehr Vorkommnisse
- `+` für eins oder mehr Vorkommnisse
- `?` für null oder eins Vorkommnisse
- `^` für das erste Zeichen in einer Zeile
- `$` für das letzte Zeichen in einer Zeile
- `[]` für den Bereich von Zeichen oder Zahlen
- `()` für Gruppierungen

Es empfiehlt sich, sich ausführlicher mit regulären Ausdrücken auseinanderzusetzen, um deren volles Potenzial ausnutzen zu können.

## Siehe auch
- [PHP-Dokumentation zu regulären Ausdrücken](http://php.net/manual/de/book.pcre.php)
- [RegExr - Online-Tool zur Erstellung und Testen von regulären Ausdrücken](https://regexr.com/)
- [Einführung in reguläre Ausdrücke auf Selfhtml](https://wiki.selfhtml.org/wiki/Shell/Regul%C3%A4rer_Ausdruck)