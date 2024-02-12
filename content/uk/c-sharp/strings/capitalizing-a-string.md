---
title:                "Зробити першу літеру рядка великою"
date:                  2024-02-03T19:06:01.435956-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Перетворення рядка з великої літери в C# полягає у зміні першого символу рядка на велику літеру, якщо він вже не є таким. Це перетворення може бути критично важливим для форматування виводу, дотримання стандартів кодування або збільшення читабельності текстів інтерфейсу користувача.

## Як це зробити:
C# пропонує простий підхід до перетворення рядків з великої літери за допомогою вбудованих методів. Найпростіший спосіб досягнення цього - змінити рядок безпосередньо за допомогою цих методів. Для більш складних або специфічних правил капіталізації (наприклад, написання кожного слова з великої літери) можуть знадобитися додаткові бібліотеки або ручні методи. Нижче наведено приклади, які демонструють, як з великої літери перетворити рядок в C# різними способами.

### Базова капіталізація:
Щоб написати з великої літери першу букву одного слова або речення:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Вивід: "Hello world"
```

### Написання кожного слова з великої літери:
Для написання з великої літери першої літери кожного слова в рядку, можна скористатися методом `TextInfo.ToTitleCase`, який знаходиться в просторі імен `System.Globalization`:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Вивід: "Hello World"
```

Примітка: `ToTitleCase` не змінює на маленькі букви решту літер; він лише перетворює на велику букву першу літеру кожного слова. Крім того, деякі слова за правилами написання заголовків (наприклад, "and", "or", "of") можуть не бути написані з великої літери в залежності від налаштувань культури.

### Використання методів розширення для можливості повторного використання:
Ви можете створити метод розширення для класу `string`, щоб спростити процес капіталізації, роблячи ваш код чистішим і більш повторно використовуваним. Ось як створити і використовувати такий метод:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Вивід: "Hello world"
    }
}
```

Цей метод розширення `Capitalize` можна викликати для будь-якого об'єкта рядка в межах простору імен, пропонуючи більш інтуїтивний та об'єктно-орієнтований підхід до маніпуляції з рядками в C#.

### Сторонні бібліотеки:
Поки стандартна бібліотека C# задовольняє більшість потреб у капіталізації рядків, певні спеціалізовані завдання можуть скористатися від сторонніх бібліотек, таких як Humanizer. Однак для завдання простого перетворення рядків або кожного слова в рядку з великої літери, стандартні методи C# є адекватними та ефективними, усуваючи потребу в зовнішніх залежностях.
