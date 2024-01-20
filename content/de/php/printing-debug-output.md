---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was und Warum?

Debugging-Output in PHP ist einfach das Drucken von Daten auf der Konsole oder dem Bildschirm, um den aktuellen Zustand eines Programms zu überprüfen. Dies hilft Programmierern dabei, Fehler oder Inkonsistenzen in ihrem Code zu identifizieren und zu beheben.

## So geht's:

PHP bietet mehrere Funktionen und Methoden zum Drucken von Debug-Ausgaben. Am bekanntesten sind `echo`, `print`, und `print_r`.

```PHP
<?php
    $a = 5;
    $b = ['a' => 'Apfel', 'b' => 'Banane'];
    
    echo $a; // Gibt 5 aus
    print $a; // Gibt ebenfalls 5 aus

    print_r($b); 
    /*
    Ausgabe:
    Array
    (
        [a] => Apfel
        [b] => Banane
    )
    */
?>
```

## Deep Dive:

Historisch gesehen waren `echo` und `print` die go-to Methoden zum Drucken von Text in PHP. `print_r` wurde hauptsächlich zum Drucken von Array-Inhalten eingeführt. 

Darüber hinaus gibt es jedoch eine leistungsfähigere Funktion namens `var_dump`, die neben dem Drucken von Arrays auch Informationen über den Typ und die Größe der Daten liefert (sehr nützlich für das Debugging).

Alternativ bietet auch die xdebug-Erweiterung von PHP erweiterte Debugging-Funktionen, darunter das "schöne" Drucken von Variablen mit `var_dump`.

```PHP
<?php
    var_dump($b);
    /*
    Ausgabe:
    array(2) {
      ["a"]=>
      string(5) "Apfel"
      ["b"]=>
      string(6) "Banane"
    }
    */
?>
```
 
Im Produktionscode sollten Debug-Ausgaben jedoch deaktiviert oder entfernt werden, da sie dazu führen können, dass sensible Informationen offenbart werden.

## Siehe auch:

- [PHP echo und print Statements](https://www.php.net/manual/de/function.echo.php)
- [PHP print_r Funktion](https://www.php.net/manual/de/function.print-r.php)
- [PHP var_dump Funktion](https://www.php.net/manual/de/function.var-dump.php)
- [Xdebug, eine PHP-Erweiterung für Debugging](https://xdebug.org/)