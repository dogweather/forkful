---
title:                "C#: Tests schreiben"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Warum sollten Sie Tests schreiben, wenn Sie bereits Code schreiben? Ganz einfach: Tests helfen Ihnen, sicherzustellen, dass Ihr Code richtig funktioniert und dass Änderungen oder Erweiterungen keine unerwarteten Nebeneffekte haben.

## Wie

Das Schreiben von Tests in C# ist ganz einfach. Hier ist ein einfaches Beispiel:

```C#
using System;
using Xunit;

namespace TestExamples
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_TwoNumbers_ReturnsCorrectSum()
        {
            // Arrange
            Calculator calculator = new Calculator();

            // Act
            int sum = calculator.Add(5, 10);

            // Assert
            Assert.Equal(15, sum);
        }
    }

    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }
    }
}
```

In diesem Beispiel haben wir eine einfache Klasse `Calculator`, die eine Methode `Add` hat, die zwei Zahlen addiert und das Ergebnis zurückgibt. Um zu überprüfen, ob diese Methode richtig funktioniert, haben wir einen Test geschrieben, der erwartet, dass die Summe von 5 und 10 15 ergibt.

Um Tests in C# zu schreiben, verwenden wir das Framework xUnit und die Annotation `[Fact]`, um eine Testmethode zu kennzeichnen. Innerhalb des Tests führen wir die Aktion aus, die wir testen möchten, und überprüfen dann das Ergebnis mit Hilfe der Assert-Methode.

Sie können auch Parameterübergabe, Ausnahmen und viele andere Dinge in Ihren Tests überprüfen. Es gibt viele Tutorials und Ressourcen im Internet, die Ihnen helfen können, mehr über das Schreiben von Tests in C# zu erfahren.

## Deep Dive

Während es wichtig ist, Tests zu schreiben, ist es auch wichtig, gute Tests zu schreiben. Hier sind einige Tipps:

- Schreiben Sie unabhängige Tests: Stellen Sie sicher, dass Ihre Tests unabhängig voneinander sind und nicht aufeinander aufbauen. Auf diese Weise können Sie sicherstellen, dass ein fehlgeschlagener Test auf ein bestimmtes Problem hinweist, anstatt dass mehrere Tests gleichzeitig fehlschlagen.

- Testen Sie alle Szenarien: Versuchen Sie, alle möglichen Szenarien und Bedingungen in Ihren Tests abzudecken, um sicherzustellen, dass Ihr Code in allen Fällen richtig funktioniert.

- Überprüfen Sie die Randfälle: Achten Sie besonders auf Randfälle und stellen Sie sicher, dass Ihr Code auch in diesen Situationen das erwartete Verhalten aufweist.

- Halten Sie Ihre Tests wartbar: Schreiben Sie lesbare und verständliche Tests, damit sie auch in Zukunft leicht zu warten sind.

Durch das Schreiben guter Tests können Sie sicherstellen, dass Ihr Code zuverlässig und fehlerfrei läuft und somit auch die Qualität Ihrer Anwendung verbessern.

## Siehe auch

- [xUnit Dokumentation](https://xunit.net/docs/getting-started/netcore/cmdline)
- [C# Test Tutorial von Microsoft](https://docs.microsoft.com/en-us/dotnet/core/testing/)
- [Unit Testing in C# mit NUnit](https://www.tutorialspoint.com/nunit/nunit_introduction.htm)