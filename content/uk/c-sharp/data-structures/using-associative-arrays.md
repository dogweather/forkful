---
title:                "Використання асоціативних масивів"
aliases:
- /uk/c-sharp/using-associative-arrays.md
date:                  2024-01-30T19:10:19.455657-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання асоціативних масивів"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Асоціативні масиви або словники у C# дозволяють зберігати та керувати парами ключів та значень. Вони є вашим головним інструментом, коли потрібно швидко отримувати значення на основі унікального ідентифікатора, що полегшує управління даними у складних додатках.

## Як:

У C#, ви працюєте з асоціативними масивами за допомогою класу `Dictionary<TKey, TValue>`. Ось швидкий приклад для початку:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Створення словника
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // Додавання пар ключ-значення
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // Доступ до значення за допомогою ключа
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // Оновлення значення
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // Видалення пари ключ-значення
        fruitBasket.Remove("Oranges");

        // Ітерація через словник
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
Приклад виводу:
```
Apples: 5
Updated Apples: 7
Apples: 7
```

Цей приклад демонструє створення словника, додавання, доступ, оновлення та видалення елементів та ітерацію через нього.

## Поглиблено

Концепція асоціативних масивів сягає їх використання в скриптових мовах, як-от Perl та PHP, де вони пропонують гнучкість у керуванні колекціями даних. У C#, `Dictionary<TKey, TValue>` є фактичною реалізацією, введеною в .NET Framework 2.0. Він зберігає дані в хеш-таблиці, забезпечуючи ефективні пошуки, додавання та видалення.

Однак варто зазначити, що хоча словники надзвичайно універсальні, вони можуть не завжди бути найкращим вибором. Для утримання впорядкованих колекцій можна розглянути `SortedDictionary<TKey, TValue>` або `SortedList<TKey, TValue>`, які пропонують сортований порядок за ціною повільніших операцій вставки та видалення. Для сценаріїв, що вимагають потокової безпеки, `ConcurrentDictionary<TKey, TValue>` додає накладні витрати, але забезпечує безпечний доступ з кількох потоків без ручного блокування.

Врешті-решт, вибір реалізації асоціативного масиву в C# залежить від ваших конкретних потреб стосовно порядку, продуктивності та безпеки потоків.
