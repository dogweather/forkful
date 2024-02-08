---
title:                "Генерація випадкових чисел"
aliases:
- uk/c-sharp/generating-random-numbers.md
date:                  2024-01-27T20:33:18.003623-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерація випадкових чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?

Генерація випадкових чисел у C# полягає у створенні непередбачуваних числових значень у вказаному діапазоні. Програмісти використовують ці методи для реалізації таких функцій, як криптографія, симуляції та ігри, де необхідна непередбачуваність або симуляція реальної випадковості.

## Як це зробити:

Найпоширеніший спосіб генерації випадкових чисел у C# - це використання класу `System.Random`. Ось простий приклад, який демонструє його використання:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Генерує число між 1 та 99
        Console.WriteLine($"Випадкове число: {randomNumber}");
    }
}
```

Це призведе до виведення випадкового числа, такого як:

```
Випадкове число: 42
```

Для генерації випадкового числа з плаваючою точкою між 0,0 та 1,0, ви можете використати метод `NextDouble`:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Випадкове подвійне: {randomDouble}");
```

Якщо ви працюєте над додатком, чутливим до безпеки, який вимагає криптографічної випадковості, краще використовувати клас `RNGCryptoServiceProvider`, який знаходиться у `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Створює 4-байтове довге випадкове число
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Криптографічно безпечне випадкове число: {value}");
    }
}
```

## Поглиблене розглядання

Генерація випадкових чисел у C# з часом розвивалася. Спочатку клас `System.Random` був основним для генерації псевдовипадкових чисел. Він є псевдовипадковим, тому що, отримавши конкретне початкове значення, він виробить ту саму послідовність чисел, що може бути корисним для налагодження або повторюваності тестів.

Незважаючи на те, що `System.Random` достатньо для базових потреб, він не є потокобезпечним і може створювати передбачувані результати, що неприпустимо для додатків, залежних від безпеки. Це обмеження призвело до введення `RNGCryptoServiceProvider` для криптографічної випадковості, який є більш безпечним, але також більш ресурсоємним.

Альтернативою у .NET Core та .NET 5+ є клас `RandomNumberGenerator` у `System.Security.Cryptography` для безпечної генерації випадкових чисел, що розглядається як більш сучасний і зручний у використанні варіант порівняно з `RNGCryptoServiceProvider`.

Кожен метод генерації випадкових чисел у C# має своє місце залежно від вимог до додатку. Для більшості додатків достатньо `System.Random`, але для тих, що вимагають безпечних, непередбачуваних випадкових чисел, криптографічні класи надають надійну альтернативу.
