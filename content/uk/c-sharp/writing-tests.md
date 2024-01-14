---
title:    "C#: Написання тестів"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Чому

Написання тестів є необхідним етапом в розробці програмного забезпечення. Він дозволяє переконатися, що ваш код працює правильно та забезпечує високу якість продукту. Також тести зменшують кількість помилок та полегшують процес випробування програми.

## Як

Написання тестів у C# може здатися складною та часом важкою задачею, але насправді це просто і потребує ретельности та деяких знань. Для початку, нам потрібно деяке програмне забезпечення: Visual Studio, NUnit Framework та MSTestAdapter. Тепер ознайомимося з прикладом написання тестів з виведенням результатів в консоль.

```C#
using NUnit.Framework;
using System;

namespace UnitTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_TwoPositiveIntegers_ReturnsCorrectResult()
        {
            // Arrange
            int num1 = 5;
            int num2 = 10;
            int expectedResult = 15;

            // Act
            Calculator calculator = new Calculator();
            int result = calculator.Add(num1, num2);

            // Assert
            Assert.AreEqual(expectedResult, result);
            Console.WriteLine("Result of adding {0} and {1} is {2}", num1, num2, result);
        }
    }
}
```

Вивід: "Result of adding 5 and 10 is 15"

## Глибший огляд

Написання тестів передбачає створення окремого проекту для тестування, де будуть розміщені всі тести. Під час написання тестів, ми повинні переконатися, що кожен метод тестує лише одну функцію. Також важливо визначити очікуваний результат та перевірити його з фактичним. Таким чином, ми можемо виявити помилки та швидко їх виправити.

Також варто згадати про перевірку стану об'єкту під час тестування. Ідея полягає в тому, щоб мати єдиний вихідний стан для кожного тесту, щоб уникнути залежностей між тестами. Це забезпечує стабільність та надійність ваших тестів.

## Дивись також

- [Як писати ефективні тести у C#](https://docs.microsoft.com/en-us/dotnet/core/testing/index)
- [Туторіал по написанню тестів у Visual Studio](https://docs.microsoft.com/en-us/visualstudio/test/writing-unit-tests-for-csharp?view=vs-2019)
- [Рахувальна машина у список покупок](https://github.com/olivierrouse/exercism/tree/master/exercises/csharp/bob)