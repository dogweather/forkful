---
title:                "C#: Pisanie testów"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

# Dlaczego pisanie testów jest ważne w programowaniu?

Pisanie testów jest nieodłączną częścią procesu pisania oprogramowania. Sprawdzenie, czy kod działa poprawnie i ma poprawne wyniki jest nie tylko pomocne w znalezieniu i naprawieniu błędów, ale też pozwala na utrzymanie wysokiej jakości kodu. W tym wpisie dowiesz się, jak napisać skuteczne testy w języku C#.

## Jak to zrobić?

Najważniejszym krokiem w pisaniu testów jest wybranie odpowiedniego narzędzia. W języku C# popularnym wyborem jest framework NUnit. Poniżej przedstawiamy przykładowy kod testu przy użyciu tego narzędzia:

```C#
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
  [Test]
  public void Add_ShouldReturnCorrectResult()
  {
    // Arrange
    var calculator = new Calculator();
    
    // Act
    var result = calculator.Add(5, 10);
    
    // Assert
    Assert.AreEqual(15, result);
  }
}
```

W powyższym przykładzie tworzymy klasę zawierającą testy oraz metodę testującą dodawanie dwóch liczb przy użyciu klasy Calculator. W fazie "Arrange" tworzymy instancję tej klasy, a w fazie "Act" wywołujemy metodę Add z odpowiednimi parametrami. Następnie w fazie "Assert" sprawdzamy, czy zwrócony wynik jest zgodny z oczekiwanym.

Pamiętaj, że testy powinny być pisane w taki sposób, by były możliwie najbardziej niezależne od siebie, czyli testy nie powinny zależeć od wyników innych testów.

## Głębszy przegląd

Niezbędne jest, aby testy były łatwe w utrzymaniu i modyfikowaniu. Dlatego ważne jest, aby pisać testy tak, jakbyśmy pisali normalny kod. Identyfikowanie odpowiednich asercji i testowanie krytycznych ścieżek kodu jest kluczowe dla skutecznych testów.

Istotną rzeczą jest też to, aby testy były pisane przed napisaniem właściwego kodu. Nazywa się to podejściem TDD (Test-Driven Development) i pomaga w tworzeniu lepszego i lepiej przetestowanego kodu.

## Zobacz też

- [Dokumentacja NUnit](https://nunit.org/)
- [Podstawowe zasady pisania testów w C#](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-best-practices)
- [Wpływ pisania testów na jakość kodu](https://blog.codinghorror.com/i-pity-the-fool-who-doesnt-write-unit-tests/)