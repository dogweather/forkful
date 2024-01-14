---
title:    "PHP: Tests schreiben"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Warum Tests schreiben?

Tests sind ein wichtiger Bestandteil des Programmierens, da sie sicherstellen, dass der Code wie erwartet funktioniert. Sie helfen dabei, Fehler zu finden und zu beheben, bevor sie in die Produktionsumgebung gelangen. Tests ermöglichen auch eine bessere Strukturierung des Codes und erleichtern es anderen Entwicklern, diesen zu verstehen und damit zu arbeiten.

## Wie man Tests schreibt

Das Schreiben von Tests in PHP ist relativ einfach und kann durch die Verwendung von Tools wie PHPUnit vereinfacht werden. Im Folgenden finden Sie ein Beispiel für das Schreiben eines einfachen Tests für eine Funktion, die zwei Zahlen addiert:

```PHP
/**
 * Test für die Additionsfunktion
 */
public function test_addition() {
  $sum = addNumbers(5, 10);
  $this->assertEquals(15, $sum);
}
```

In diesem Beispiel wird die assertEquals-Funktion verwendet, um sicherzustellen, dass die Ausgabe der Funktion tatsächlich die erwartete Summe von 15 ist. Durch das Schreiben von solchen Tests können potenzielle Fehler im Code frühzeitig erkannt und behoben werden.

## Tiefer Tauchgang

Tests sollten immer vollständig und unabhängig voneinander sein, um sicherzustellen, dass sie zuverlässige Ergebnisse liefern. Es ist auch wichtig, sowohl positive als auch negative Fälle zu testen, um sicherzustellen, dass der Code in allen Szenarien korrekt funktioniert. Durch den Einsatz von Test-Driven-Development, bei dem Tests vor der eigentlichen Implementierung geschrieben werden, kann die Qualität des Codes weiter verbessert werden.

# Siehe auch

- [PHPUnit Dokumentation](https://phpunit.de/documentation.html)
- [Test-driven development mit PHP](https://www.phparch.com/2019/05/confessions-of-a-test-driven-developer/)
- [Warum Tests wichtig sind](https://www.sitepoint.com/why-test-units-best-practices-php-programs/)