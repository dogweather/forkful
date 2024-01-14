---
title:    "PHP: Tests schreiben"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-tests.md"
---

{{< edit_this_page >}}

## Warum
Tests sind ein integraler Bestandteil der Softwareentwicklung, der oft übersehen oder vernachlässigt wird. Doch das Schreiben von Tests kann dazu beitragen, die Qualität und Stabilität einer Anwendung zu verbessern. In diesem Blogbeitrag werden wir uns ansehen, warum es wichtig ist, Tests in PHP-Anwendungen zu schreiben.

## Wie
Um mit dem Schreiben von Tests in PHP zu beginnen, müssen Sie zunächst eine Testframework wie PHPUnit installieren. Dann können Sie Ihre Tests in Methoden verpacken und Assertions hinzufügen, um die erwarteten Ergebnisse zu überprüfen. Hier ist ein Beispiel für das Testen einer einfachen Additionsfunktion:

```PHP
<?php
// zu testende Funktion
function add($a, $b)
{
    return $a + $b;
}

// Testklasse
class AddTest extends PHPUnit_Framework_TestCase
{
    public function testAdd()
    {
        // Arrange
        $a = 5;
        $b = 10;

        // Act
        $result = add($a, $b);

        // Assert
        $this->assertEquals(15, $result);
    }
}
```

In diesem Beispiel haben wir eine einfache Funktion "add" geschrieben, die zwei Zahlen addiert. Dann haben wir eine Testklasse erstellt, die von PHPUnit_Framework_TestCase erbt und eine Testmethode "testAdd" enthält. Dort haben wir die benötigten Variablen initialisiert, die Funktion aufgerufen und mit einer Assertion überprüft, ob das Ergebnis der erwarteten Summe entspricht.

## Deep Dive
Es gibt viele verschiedene Arten von Tests in der Softwareentwicklung, wie z.B. Unit-Tests, Integrationstests und Akzeptanztests. Unit-Tests konzentrieren sich auf das Testen von einzelnen Funktionen oder Methoden, während Integrationstests sich mit der Zusammenarbeit von Komponenten beschäftigen. Akzeptanztests überprüfen, ob die Anwendung die vom Kunden gewünschten Anforderungen erfüllt.

Beim Schreiben von Tests ist es wichtig, eine gute Coverage zu erreichen, d.h. sicherzustellen, dass möglichst alle Codepfade getestet werden. Eine Coverage von 100% zu erreichen ist zwar nicht immer realistisch, aber es lohnt sich, sich konstant zu verbessern.

Ein weiterer wichtiger Aspekt des Testens ist das Mocking, d.h. das Ersetzen von Abhängigkeiten wie Datenbankzugriffe oder externe APIs durch simulierten Verhaltensweisen. Dies ermöglicht es, Tests unabhängig voneinander durchzuführen und die Anwendung schneller und zuverlässiger zu testen.

## Siehe auch
- [PHPUnit Dokumentation](https://phpunit.de/documentation.html)
- [Test Driven Development mit PHPUnit](https://phpunit.de/getting-started/test-driven-development.html)
- [10 Gründe, warum Tests wichtig sind](https://www.pmg.com/blog/10-reasons-why-testing-matters/)
- [Test-Driven Development für Einsteiger](https://www.codeproject.com/Articles/1082055/Unit-Testing-for-Beginners)