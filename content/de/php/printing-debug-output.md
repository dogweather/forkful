---
title:                "Debug-Ausgabe drucken"
html_title:           "PHP: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Programmieren kommt es manchmal vor, dass man nicht sofort erkennt, was der Code macht oder wo ein Fehler auftritt. Deshalb verwenden Programmierer oft eine Technik namens "Debug Ausgabe", um Fehler zu finden und den Code zu verstehen.

## So geht's:

Um eine Debug Ausgabe in PHP zu erstellen, kannst du einfach ```print_r()``` oder ```var_dump()``` verwenden. Schreibe einfach den Code, den du überprüfen möchtest in die Klammern und das Ergebnis wird auf deinem Bildschirm ausgegeben. Zum Beispiel:

```PHP 
// Variablen definieren
$alter = 30;
$name = "Anna";

// Debug Ausgabe
print_r($alter);
var_dump($name);
```

Wenn du diesen Code ausführst, siehst du, dass deine Variablen und ihre Werte ausgegeben werden. Dadurch kannst du überprüfen, ob deine Variablen korrekt zugewiesen wurden und den Code besser verstehen.

## Tiefere Einblicke:

Die Idee der Debug Ausgabe ist nicht neu. Schon früher haben Programmierer ähnliche Techniken verwendet, um ihre Programme zu überprüfen. Es gibt auch alternative Methoden, wie zum Beispiel das Einbinden von Debugger-Modulen in deinen Code oder das Verwenden von speziellen Code-Analyse-Tools.

Es ist wichtig zu wissen, dass Debug Ausgaben im Produktivcode nicht empfohlen werden, da sie die Leistung der Anwendung beeinträchtigen können. Sie sollten also nur für Entwicklungs- und Testzwecke verwendet werden.

## Siehe auch:

- [PHP Debugging Functions](https://www.php.net/manual/en/ref.debugger.php)
- [PHP Debugging Tips](https://www.php.net/manual/en/debugger-tips.php)