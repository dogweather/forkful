---
title:                "Створення випадкових чисел"
html_title:           "C#: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Що & Чому?
Генерування випадкових чисел - це процес створення чисел у випадковому порядку, без будь-якої специфіки або патерну. Це часто використовується програмістами для створення тестових даних, шифрування і генерації унікальних ідентифікаторів.

# Як це зробити:
```C#
// Створення випадкового числа за допомогою класу Random
Random r = new Random();

// Генерування випадкового цілого числа від 1 до 10
int randomNumber = r.Next(1, 11); 

// Генерування випадкового числа з дробовою частиною від 0 до 1
double randomDouble = r.NextDouble(); 

// Виведення результату
Console.WriteLine(randomNumber); 
Console.WriteLine(randomDouble);
```

### Вихід:
```
6
0.674186751
```

# Глибока піронинг:
Генерування випадкових чисел було розроблено в 1946 році вченим Джоном фон Нейманом для використання в машині ЕНІАК. В даний час існують різноманітні алгоритми генерування випадкових чисел, включаючи методи, які використовують шум, заснований на апаратурі комп'ютера, або на основі математичних формул.

Є також альтернативи для генерування випадкових чисел, такі як сторонні бібліотеки, наприклад, Universal Unique Identifier (UUID), які генерують унікальні ідентифікатори.

У C# для генерування випадкових чисел можна використовувати клас Random або вбудований метод Next(). Клас Random має можливість генерувати як випадкові цілі, так і з дробовою частиною. Вбудований метод Next() використовується для генерування випадкових цілих чисел.

# Дивіться також:
- [Random.Next Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.random.next?view=net-5.0)
- [Generating Random Numbers with C#](https://www.c-sharpcorner.com/article/generating-random-numbers-in-c-sharp/)
- [Alternative Random Number Generators in C#](https://www.codeproject.com/Articles/470921/Alternative-Random-Number-Generators)