---
title:                "Das Drucken von Debug-Ausgabe"
html_title:           "PHP: Das Drucken von Debug-Ausgabe"
simple_title:         "Das Drucken von Debug-Ausgabe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum
 
Debug-Ausgaben sind ein nützliches Werkzeug für Entwickler, um Probleme in ihrem Code zu identifizieren und zu beheben. Durch das Drucken von Debug-Ausgaben können Sie die Werte von Variablen und anderen wichtigen Informationen in Echtzeit überprüfen, was die Fehlerbehebung erleichtert. Es ist auch eine einfache Möglichkeit, den Verlauf des Programms zu verfolgen und zu verstehen, wie es funktioniert.

## Wie man Debug-Ausgaben in PHP druckt

Um Debug-Ausgaben in PHP zu drucken, verwenden Sie einfach die Funktion `echo` oder `print`, gefolgt von den zu druckenden Variablen oder Informationen. Hier ist ein Beispiel:

```PHP
$name = "Max";
$age = 28;

echo "Name: " . $name;
print "Alter: " . $age;
```
Die Ausgabe wäre:

```
Name: Max
Alter: 28
```

Sie können auch die `var_dump()` oder `print_r()` Funktionen verwenden, um die Werte von Variablen und Arrays ausführlich anzuzeigen. Hier ist ein Beispiel:

```PHP
$cities = ["Berlin", "Hamburg", "Munich"];

var_dump($cities);
print_r($cities);
```
Die Ausgabe könnte so aussehen:

```
array(3) {
    [0]=> string(6) "Berlin"
    [1]=> string(7) "Hamburg"
    [2]=> string(6) "Munich"
}
Array
(
    [0] => Berlin
    [1] => Hamburg
    [2] => Munich
)
```

Es ist auch möglich, die debug_backtrace() Funktion zu verwenden, um eine detaillierte Spur der Funktionen und Dateien zu erhalten, die vor dem aktuellen Funktionsaufruf ausgeführt wurden. Ein Beispiel:

```PHP
function foo() {
    bar();
}

function bar() {
    var_dump(debug_backtrace());
}

foo();
```
Die Ausgabe wäre:

```
array(2) {
    [0]=> array(4) {
        ["file"]=> string(27) "example.php"
        ["line"]=> int(6)
        ["function"]=> string(3) "bar"
        ["args"]=> array(0) {
        }
    }
    [1]=> array(4) {
        ["file"]=> string(27) "example.php"
        ["line"]= > int(10)
        ["function"]=> string(3) "foo"
        ["args"]=> array(0) {
        }
    }
}
```

## Tiefer Einblick

Es gibt auch andere Möglichkeiten, Debug-Ausgaben zu drucken, wie z.B. die Verwendung von Log-Dateien oder speziellen Debugging-Tools. Es ist wichtig zu beachten, dass Debug-Ausgaben nicht in der Produktionsumgebung verwendet werden sollten, da sie die Leistung beeinträchtigen und vertrauliche Informationen über den Code offenlegen können.

Eine weitere wichtige Überlegung bei der Nutzung von Debug-Ausgaben ist die Verwendung von Bedingungen oder Schleifen, um sicherzustellen, dass die Ausgaben nur in bestimmten Situationen erfolgen, um den Code nicht unnötig zu verlangsamen.

## Siehe auch

- [Offizielle PHP-Dokumentation zu Debuggen](https://www.php.net/manual/de/debugger.php)
- [10 Tipps für effektives Debugging in PHP](https://www.smashingmagazine.com/2011/03/ten-useful-techniques-to-help-your-users-in-troubleshooting-php-applications/)
- [PHP Debug Console - ein nützliches Debugging-Tool für PHP-Entwickler](https://github.com/sojexx/php-console)