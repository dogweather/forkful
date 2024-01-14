---
title:    "C#: Tests schreiben"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie C# programmieren, sind Sie wahrscheinlich daran gewöhnt, Ihren Code zu schreiben und zu testen, indem Sie ihn manuell ausführen und überprüfen, ob alles wie erwartet funktioniert. Aber wussten Sie, dass es eine viel effektivere Möglichkeit gibt, Ihre Anwendung zu testen? Mit automatisierten Tests können Sie sicherstellen, dass alle Funktionen Ihrer Anwendung ordnungsgemäß funktionieren, auch nachdem Sie Änderungen am Code vorgenommen haben.

## Wie man Tests schreibt

Das Schreiben von Tests in C# ist einfach und erfordert nicht viel zusätzlichen Aufwand. Für jeden Teil Ihrer Anwendung, sei es eine Methode, eine Klasse oder ein gesamtes Modul, können Sie eine entsprechende Testklasse erstellen. In dieser Klasse können Sie verschiedene Testmethoden schreiben, die die verschiedenen Szenarien Ihrer Anwendung abdecken.

```C#
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Add_TwoNumbers_ReturnSum()
    {
        // Arrange
        var calculator = new Calculator();
        
        // Act
        var result = calculator.Add(2, 3);
        
        // Assert
        Assert.AreEqual(5, result);
    }
}
```

In diesem Beispiel erstellen wir eine Testklasse für eine einfache Rechneranwendung und schreiben eine Testmethode, die prüft, ob die Methode "Add" korrekt funktioniert, indem sie zwei Zahlen addiert. Mit dem "Assert" -Statement können wir überprüfen, ob die tatsächliche Ausgabe dem erwarteten Wert entspricht.

## Tiefergehender Einblick

Das Schreiben von Tests hilft nicht nur dabei, Fehler in Ihrer Anwendung zu finden, sondern es fördert auch eine bessere Code-Qualität. Indem Sie sich darauf konzentrieren, Tests zu schreiben, zwingen Sie sich, Ihren Code besser zu strukturieren und zu planen. Außerdem machen Tests es viel einfacher, Änderungen am Code vorzunehmen, da Sie immer sicher sein können, dass keine unerwarteten Nebenwirkungen auftreten.

Darüber hinaus gibt es verschiedene Arten von Tests, wie z.B. Unit-Tests, Integrationstests und End-to-End-Tests, die jeweils einen anderen Zweck haben. Eine tiefere Auseinandersetzung mit diesen Tests kann Ihnen helfen, eine noch umfassendere Testabdeckung für Ihre Anwendung zu erreichen.

## Siehe auch

- [Einführung in das Testen von C# Anwendungen mit NUnit](https://docs.microsoft.com/de-de/dotnet/core/testing/unit-testing-with-nunit?tabs=visual-studio)
- [Effektives Testen von C# Anwendungen mit XUnit](https://xunit.net/docs/getting-started/netcore/cmdline)
- [Test Driven Development in C#](https://blog.submain.com/getting-started-with-test-driven-development-using-csharp/)