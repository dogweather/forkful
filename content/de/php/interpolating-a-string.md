---
title:                "Eine Zeichenfolge interpolieren"
html_title:           "PHP: Eine Zeichenfolge interpolieren"
simple_title:         "Eine Zeichenfolge interpolieren"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was ist String-Interpolation und warum verwenden Programmierer es?
String-Interpolation ist ein Konzept in der Programmierung, bei dem Variablen innerhalb eines Strings direkt eingesetzt werden können. Zum Beispiel kannst du den Satz "Mein Name ist [name]" erstellen, und den Platzhalter '[name]' durch die aktuelle Variable ersetzen. Programmierer verwenden String-Interpolation, um effizienter und lesbarer Code zu schreiben, da es unnötiges Schreiben von Konkatenations- und Escaping-Operatoren vermeidet.

## Wie funktioniert es?
Mit PHP ist String-Interpolation sehr einfach. Du musst nur den String mit doppelten Anführungszeichen ("") statt einfachen Anführungszeichen ('') schreiben und die Variable innerhalb des Strings mit dem Syntax `$variablenname` einfügen. Hier ist ein Beispiel:

```php
$name = "Max";
echo "Mein Name ist $name"; // gibt "Mein Name ist Max" aus
```

Wenn du auf eine ganze Variable verzichten möchtest und nur einen Teilwert ausgeben möchtest, kannst du den Arraysyntax verwenden, um auf einen Elementwert zuzugreifen:

```php
$datum = ['Tag' => 1, 'Monat' => 3, 'Jahr' => 2021];
echo "Heute ist der {$datum ['Tag']}.{$datum ['Monat']}.{$datum ['Jahr']}"; // gibt "Heute ist der 1.3.2021" aus
```

## Tiefergehende Einblicke
String-Interpolation hat seit den frühen Tagen der Programmierung an Popularität gewonnen. Früher wurde es als Teil des "Heredoc"-Syntax bezeichnet, bevor es in die Sprache integriert wurde. Es gibt auch alternative Methoden, um Strings in PHP zu interpolieren, wie z.B. eine spezielle Funktion namens `sprintf()`, die ähnliche Ergebnisse erzielt.

String-Interpolation ist auch in anderen Programmiersprachen wie Ruby und Python verfügbar. Allerdings hat jedes Sprache seine eigene Syntax und Implementierungsdetails.

## Weitere Informationen
Wenn du mehr über String-Interpolation in PHP lernen möchtest, findest du in der offiziellen Dokumentation weitere Informationen und Beispiele: https://www.php.net/manual/de/language.types.string.php#language.types.string.syntax.double

Außerdem kannst du dir die verschiedenen String-Operatoren anschauen, um zu sehen, wie sie sich von String-Interpolation unterscheiden: https://www.php.net/manual/de/language.operators.string.php