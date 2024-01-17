---
title:                "Tests schreiben"
html_title:           "PHP: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests schreiben ist eine wichtige Aufgabe in der Programmierung. Dabei werden automatisierte Tests erstellt, um sicherzustellen, dass der Code wie erwartet funktioniert und alle zugehörigen Anforderungen erfüllt werden. Programmierer machen das, um sicherzustellen, dass ihre Programme zuverlässig und fehlerfrei sind.

## Wie man es macht:

```PHP
<?php
// Beispiel einer einfachen Funktion
function add($x, $y) {
  return $x + $y;
}

// Testfall für die Funktion add()
function testAdd() {
  $x = 5;
  $y = 10;
  $expected = 15;
  $actual = add($x, $y);

  // Vergleicht das erwartete Ergebnis mit dem tatsächlichen Ergebnis
  if ($expected === $actual) {
    echo "Test erfolgreich durchgeführt!";
  } else {
    echo "Test fehlgeschlagen.";
  }
}

// Aufrufen der Testfunktion
testAdd();
```

Der Output würde hier "Test erfolgreich durchgeführt!" sein.

## Tiefer Einblick:

Tests wurden bereits in den Anfängen der Softwareentwicklung verwendet, um sicherzustellen, dass Programme korrekt funktionieren. Alternativen zu automatisierten Tests sind manuelles Testen oder Code Reviews durch andere Programmierer. Es gibt verschiedene Arten von Tests, wie z.B. unit tests, integration tests oder acceptance tests. Beim Schreiben von Tests ist es wichtig, klare und verständliche Testfälle zu erstellen und alle möglichen Szenarien abzudecken. Die Implementierung von Tests in den Entwicklungsprozess führt zu stabilerem Code und weniger Fehlern in der Anwendung.

## Siehe auch:

- [PHPUnit] (https://phpunit.de/) - Eine beliebte Test-Framework-Bibliothek für PHP
- [PHP CodeSniffer] (https://github.com/squizlabs/PHP_CodeSniffer) - Ein Code-Qualitäts-Tool, das auch auf fehlende Tests hinweist
- [The Art of Unit Testing] (https://www.manning.com/books/the-art-of-unit-testing-third-edition) - Ein empfohlenes Buch zum Thema Unit Testing in der Praxis