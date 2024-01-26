---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:05.246714-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?
Генерація випадкових чисел - це процес створення чисел, які не можна передбачити. Програмісти використовують це для ігор, симуляцій, тестування безпеки та там, де потрібна непередбачуваність.

## Як це зробити:
### Використання System.Random:
```C#
using System;

public class RandomNumberExample
{
    static void Main()
    {
        // Створюємо новий екземпляр Random.
        var random = new Random();

        // Генеруємо випадкове число від 0 до 99.
        int randomNumber = random.Next(100);
        Console.WriteLine(randomNumber);
    }
}
```
### Вивід може бути:
```
42
```

### Застосування генератора випадкових діапазонів:
```C#
// Генеруємо випадкове число між 50 та 100.
int randomInRange = random.Next(50, 101);
```

## Поглиблений аналіз:
Раніше для генерації випадкових чисел використовувались фізичні пристрої, такі як рулетки або монетки. В сучасних комп'ютерах це здійснюється програмно. Цифровий метод може включати алгоритми, які називаються псевдовипадковими чисельними генераторами (PRNGs), оскільки вони використовують детерміністичні процеси для створення чисел, які виглядяють випадковими. Одним з варіантів є System.Random в .NET, який забезпечує достатню випадковість для більшості завдань, але не є криптографічно безпечним.

Для ситуацій, які вимагають більшої безпеки, існує `System.Security.Cryptography.RandomNumberGenerator`. Цей клас використовується для створення даних, які важко передбачити, і критично важливо для криптографії.

## Дивіться також:
- Microsoft документація по `System.Random`: https://docs.microsoft.com/dotnet/api/system.random
- Microsoft документація по `RandomNumberGenerator`: https://docs.microsoft.com/dotnet/api/system.security.cryptography.randomnumbergenerator
- Вступ до криптографічних випадкових генераторів: https://csrc.nist.gov/projects/random-bit-generation
- Stack Overflow дискусія про випадкові числа в C#: https://stackoverflow.com/questions/tagged/random+c%23
