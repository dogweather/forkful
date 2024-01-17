---
title:                "Tests schreiben"
html_title:           "C#: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

C#-Programmierung: Wie man schnell und einfach Tests schreibt

## Was & Warum?
Wenn du schon einmal mit Programmierern gesprochen hast, hast du sicherlich schon einmal den Begriff "Tests" gehört. Aber was sind Tests überhaupt und warum schreiben Programmierer sie? Ganz einfach: Tests sind kleine Programme, die prüfen, ob ein größeres Programm richtig funktioniert. Programmierer schreiben Tests, um sicherzustellen, dass ihr Code fehlerfrei ist und wie erwartet arbeitet.

## Wie geht's:
Tests zu schreiben ist in C# ganz einfach! In den meisten Fällen werden sogenannte "Unit Tests" verwendet, die einzelne Methoden oder Klassen testen. Ein Beispiel für einen einfachen Unit Test kann wie folgt aussehen:

```C#
using NUnit.Framework; // Namespace für die Test-Framework-Bibliothek

[TestFixture] // Dekorator, der anzeigt, dass es sich um eine Test-Klasse handelt
public class CalculatorTests
{
  [TestCase] // Dekorator für einen einzelnen Testfall
  public void TestAddition() 
  {
    // Arrange
    var calculator = new Calculator();
    var x = 5;
    var y = 10;
    var expected = 15;

    // Act
    var actual = calculator.Add(x, y);

    // Assert
    Assert.AreEqual(expected, actual); // Assert-Statement, das prüft, ob die erwartete und tatsächliche Ausgabe übereinstimmen
  }
}
```
Und hier eine mögliche Ausgabe des Tests:

```
Tests run: 1, Failures: 0, Not run: 0, Time: 0.135 seconds
```

## Tief im Detail:
Tests zu schreiben ist mittlerweile ein fester Bestandteil der Softwareentwicklung geworden. Früher wurden Tests oft manuell durchgeführt, was jedoch sehr zeitaufwändig und fehleranfällig war. Die Verwendung von automatisierten Tests ermöglicht es Programmierern, Fehler schneller zu finden und zu beheben. Alternativ zu NUnit gibt es auch andere Test-Frameworks für C#, wie zum Beispiel xUnit oder MSTest.

Bei der Implementierung von Tests ist es wichtig, dass sie unabhängig und isoliert voneinander ausgeführt werden können. Dies bedeutet, dass jeder Testfall keine Abhängigkeit von anderen Tests haben sollte und somit auch einzeln ausgeführt werden kann. Außerdem sollten Tests leicht verständlich und gut dokumentiert sein, damit auch andere Programmierer sie einfach nachvollziehen können.

## Weitere Informationen:
Wenn du mehr über das Schreiben von Tests in C# erfahren möchtest, empfehlen wir dir folgende Quellen:

- [NUnit Dokumentation](https://github.com/nunit/docs/wiki)
- [xUnit Dokumentation](https://xunit.net/docs/getting-started/netcore/cmdline)
- [MSTest Dokumentation](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest)

So, das war's auch schon! Jetzt bist du bereit, mit dem Schreiben von Tests in C# loszulegen und dein Code wird in Zukunft sicherlich fehlerfrei sein. Viel Spaß beim Coden!