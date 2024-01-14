---
title:                "C#: Пошук та заміна тексту"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому
У програмуванні, часто буває необхідність заміни деякого тексту іншим, наприклад, для виправлення помилки чи зміни формату. Це може бути дуже часово і ресурсоємке завдання, яке може бути автоматизоване за допомогою функції пошуку та заміни тексту.

## Як це зробити
Для пошуку та заміни тексту в C# можна використовувати метод `Replace()` з класу `String`. Потрібно передати два параметри - рядок, який потрібно замінити та рядок, яким потрібно замінити.

```C#
string myText = "Привіт, світ!";
string newText = myText.Replace("світ", "друг");
Console.WriteLine(newText);
```
Вище наведений код виведе рядок "Привіт, друг!", оскільки метод `Replace()` замінив слово "світ" на "друг". 

Іншою корисною функцією є `ReplaceFirst()`, яка замінює лише перше входження замінюваного тексту.

```C#
string myText = "Привіт, світ!";
string newText = myText.ReplaceFirst("в", "у");
Console.WriteLine(newText);
```
Цей код виведе рядок "Приуіт, світ!".

## Глибше погляднути
Крім строки, метод `Replace()` може приймати також регулярний вираз, що дозволяє більш гнучко налаштувати пошук та заміну тексту. Регулярні вирази - це спеціальні шаблони, за допомогою яких можна шукати в рядку певні символи або шаблони.

Наприклад, замінимо всі цифри в рядку на символи `x`:

```C#
string myText = "456 lala 789";
string newText = Regex.Replace(myText, @"\d+", "x");
Console.WriteLine(newText);
```

Виведе рядок "x lala x".

## Подивіться також
- [Документація по класу String](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
- [Регулярні вирази в C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)