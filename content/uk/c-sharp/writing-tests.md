---
title:                "C#: Запис тестів."
simple_title:         "Запис тестів."
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Програмування тестів може здатися важливим, але насправді це є необхідною складовою процесу розробки програмного забезпечення. Тести допомагають перевірити правильність коду, виявити і виправити помилки, а також забезпечують впевненість у якості продукту.

## Як

Найпоширенішим способом написання тестів є використання фреймворку NUnit у поєднанні з Microsoft Visual Studio. Спочатку потрібно створити проект для тестів, а потім написати їх за допомогою атрибутів тестування та методів Assert.

```C#
[Test]
public void TestSum()
{
    int result = Calculator.Sum(3, 5);
    Assert.AreEqual(8, result);
}
```

Результат повинен бути подібним до цього:

```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 0.013 seconds
```

## Глибоке занурення

Для більш детального розуміння та використання тестування у своїй роботі, можна ознайомитися з різними типами тестів, такими як модульні, інтеграційні та функціональні, а також дізнатися про принципи TDD (Test-Driven Development) та BDD (Behavior-Driven Development).

## Дивись також

- [NUnit](https://nunit.org/)
- [Microsoft Docs: Тести C# для початківців](https://docs.microsoft.com/uk-ua/dotnet/core/testing/index)
- [Тести в C#: посібник розробника](https://www.lukiweb.com/csharp/csharp%20test%20methode.html)