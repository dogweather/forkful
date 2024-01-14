---
title:                "C#: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Tests ist ein wichtiger Bestandteil einer guten Softwareentwicklung. Durch das Testen unserer Code können wir sicherstellen, dass er zuverlässig funktioniert und potenzielle Fehler frühzeitig erkennen. Außerdem können Tests dabei helfen, den Code übersichtlicher und wartbarer zu machen.

## Wie Geht Es

Um Tests in C# zu schreiben, müssen wir zunächst ein Test-Framework wie zum Beispiel NUnit oder xUnit einbinden. Dann können wir unsere Tests in Klassen schreiben, die von einer Testklasse erben und mit Prüfmethoden wie Assert.AreEqual() oder Assert.IsTrue() überprüfen, ob das erwartete Ergebnis erreicht wurde.

Hier ist ein Beispiel für einen einfachen Test, der überprüft, ob die Summe von zwei Zahlen korrekt berechnet wird:

```C#
public class CalculatorTests : TestBase
{
    [Test]
    public void TestAddTwoNumbers()
    {
        // Arrange
        Calculator calculator = new Calculator();

        // Act
        int result = calculator.Add(2, 3);

        // Assert
        Assert.AreEqual(5, result);
    }
}
```

Durch das Ausführen dieses Tests erhalten wir im Idealfall eine grüne Markierung, die zeigt, dass der Test erfolgreich war. Wenn der Test fehlschlägt, wird uns genau angezeigt, an welcher Stelle der Code nicht wie erwartet funktioniert, was das Debugging erleichtert.

## Tiefer Einblick

Um effektive Tests zu schreiben, sollten wir uns mit den verschiedenen Arten von Tests auseinandersetzen, wie zum Beispiel Unit Tests, Integrations- und Akzeptanztests. Außerdem ist es wichtig, gute Praktiken wie Test Driven Development (TDD) zu erlernen, um bereits während des Schreibens von Code an Tests zu denken.

Es gibt auch verschiedene Techniken und Tools, mit denen wir unsere Tests noch effizienter gestalten können, wie zum Beispiel Mocking-Frameworks oder automatisierte Testausführung.

Insgesamt ist es wichtig, sich kontinuierlich mit dem Schreiben von Tests auseinanderzusetzen und zu verbessern, um einen qualitativ hochwertigen Code zu produzieren.

## Siehe Auch

- [xUnit.net](https://xunit.net/)
- [NUnit](https://nunit.org/)
- [Test Driven Development: By Example by Kent Beck](https://www.goodreads.com/book/show/387190.Test_Driven_Development)