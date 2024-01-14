---
title:                "C#: Створення випадкових чисел"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому

Згенерувати випадкові числа є важливою частиною багатьох програм, таких як ігри та криптографічні алгоритми. Генерація випадкових чисел забезпечує різноманітність та непередбачуваність в програмах, що робить їх більш безпечними та цікавими для користувачів.

## Як

```c#
using System;

namespace RandomNumberGenerator
{
    class Program
    {
        static void Main(string[] args)
        {
            // створення екземпляру класу Random
            Random random = new Random();

            // для генерації випадкового цілого числа використовуємо метод Next()
            int randomNumber = random.Next();

            // для генерації випадкового числа в діапазоні використовуємо метод Next(min, max)
            int randomRangeNumber = random.Next(1, 10);

            // виведення результату на екран
            Console.WriteLine("Випадкове ціле число: " + randomNumber);
            Console.WriteLine("Випадкове число в діапазоні від 1 до 10: " + randomRangeNumber);
            
            // для генерації випадкового числа з плаваючою комою використовуємо метод NextDouble()
            double randomDouble = random.NextDouble();

            // виведення результату на екран
            Console.WriteLine("Випадкове число з плаваючою комою: " + randomDouble);
        }
    }
}

```

Вище наведені приклади демонструють, як використовувати клас Random в C# для генерації випадкових чисел. Ці методи дозволяють генерувати числа різного типу та в різних діапазонах. Також можна налаштовувати початковий стан генератора випадкових чисел за допомогою конструктора класу Random.

## Глибше розуміння

Генерація випадкових чисел є складним процесом, який базується на математичних алгоритмах. У класі Random використовується лінійний конгруентний метод, який забезпечує псевдовипадковість чисел. Це означає, що отримані значення є непередбачуваними, але не є справжніми випадковими числами. Це може бути корисно для більшості програм, але для криптографічних систем рекомендується використовувати інші методи генерації.

## Дивись також

- [Microsoft документація про клас Random в C#](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1)
- [Стаття про генерацію випадкових чисел на сайті CodeProject](https://www.codeproject.com/Articles/21190/Understanding-and-Using-C-Random-Number-Generators)
- [Відеоурок про клас Random в C#](https://www.youtube.com/watch?v=MnWOCJjNgQc)