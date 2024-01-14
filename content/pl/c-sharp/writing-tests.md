---
title:    "C#: Pisanie testów."
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

# Dlaczego warto pisać testy w C#?

Testowanie jest nieodzownym elementem procesu tworzenia oprogramowania. Jest to potwierdzenie, że nasz kod działa prawidłowo i spełnia założone wymagania. Pisanie testów w języku C# jest kluczowym krokiem w zapewnieniu jakości naszego kodu. W tym artykule omówimy, dlaczego warto pisać testy w C# oraz jak to zrobić.

## Jak pisać testy w C#

Aby napisać testy w języku C#, należy w pierwszej kolejności zdefiniować przestrzeń nazw "using NUnit.Framework;". Następnie, przy użyciu atrybutów "TestFixture" i "Test", można utworzyć nowe klasy i metody testowe. Poniższy przykład pokazuje, jak przetestować metodę dodawania:

```C#
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Add_TwoNumbers_ReturnsCorrectSum()
    {
        // arrange
        Calculator calc = new Calculator();
        int num1 = 2;
        int num2 = 3;
        int expectedSum = 5;

        // act
        int actualResult = calc.Add(num1, num2);

        // assert
        Assert.AreEqual(expectedSum, actualResult);
    }
}
```

W powyższym przykładzie, najpierw tworzymy instancję klasy "Calculator", która zawiera metodę dodawania. Następnie definiujemy dwie zmienne, które będą argumentami metody "Add". W kolejnym kroku, wykonujemy metodę "Add" na obiekcie "calc", a następnie sprawdzamy czy otrzymany wynik jest zgodny z oczekiwanym.

## Głębszy wgląd w pisanie testów

Pisanie testów w C# wymaga znajomości różnych metod asercji, takich jak "Assert.AreEqual()" czy "Assert.IsTrue()". Warto także zapoznać się z pojęciem mockowania obiektów przy użyciu biblioteki Moq. Pozwala to na symulowanie różnych scenariuszy i testowanie przypadków brzegowych. Należy również pamiętać o stosowaniu dobrej praktyki test-driven development (TDD), czyli pisaniu testów przed kodem właściwym.

# Zobacz również

- [Tutorial: NUnit](https://nunit.org/)
- [Moq documentation](https://www.nuget.org/packages/Moq/)
- [Test-driven development: A Practical Guide](https://www.amazon.com/Test-Driven-Development-Microsoft-Professional/dp/0735619484)