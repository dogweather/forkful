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

## Warum

Testen ist ein wichtiger Teil der Softwareentwicklung, da es uns hilft, sicherzustellen, dass unser Code zuverlässig und fehlerfrei ist. Durch das Schreiben von Tests können wir auch verhindern, dass Änderungen im Code unerwartete Nebenwirkungen haben. Es mag zwar zeitaufwändig sein, Tests zu schreiben, aber es lohnt sich, um sicherzustellen, dass unsere Anwendungen erfolgreich sind.

## Wie geht es

Um Tests in C# zu schreiben, verwenden wir das eingebaute Testframework namens "NUnit". Zuerst müssen wir "NUnit" über den NuGet-Paket-Manager in unser Projekt hinzufügen. Dann können wir innerhalb unserer Testklasse verschiedene Methoden verwenden, wie zum Beispiel `Assert`, um zu überprüfen, ob die erwarteten Ergebnisse erzielt werden. Hier ist ein Beispiel:

```
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void TestAddition()
    {
        // Arrange
        int num1 = 5;
        int num2 = 10;
        
        // Act
        int result = num1 + num2;
        
        // Assert
        Assert.AreEqual(15, result);
    }
}
```

In diesem Beispiel erstellen wir eine Testklasse namens "CalculatorTests" und verwenden die Methode `Test` von NUnit, um unsere Testmethode zu markieren. Innerhalb dieser Methode führen wir unsere Berechnung aus und verwenden dann `Assert.AreEqual`, um zu überprüfen, ob das Ergebnis dem erwarteten Ergebnis entspricht. Durch das Ausführen dieser Tests können wir sicherstellen, dass unsere "Calculator"-Klasse korrekt funktioniert.

## Tiefer eintauchen

Es gibt viele Möglichkeiten, Tests zu schreiben und zu organisieren, je nach den spezifischen Anforderungen und Vorlieben jedes Entwicklers. In C# können wir auch verschiedene "Test Runner" wie "Test Explorer" oder "ReSharper" verwenden, um unsere Tests auszuführen und die Ergebnisse anzuzeigen. Außerdem gibt es Konzepte wie "Test-Driven Development" und "Behavior-Driven Development", die eine andere Herangehensweise an das Schreiben von Tests bieten.

## Siehe auch

- [NUnit-Dokumentation] (https://nunit.org/docs/)
- [Microsoft-Dokumentation zu Tests in C#] (https://docs.microsoft.com/de-de/dotnet/core/testing/)
- [Artikel über Test-Driven Development in C#] (https://dzone.com/articles/test-driven-development-with-csharp)