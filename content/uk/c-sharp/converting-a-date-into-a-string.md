---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що & чому?
Перетворення дати в рядок - це процес форматування дати в рядок, який можна легко зрозуміти та відобразити. Програмісти роблять це для гнучкого відображення дат на веб-сайтах, в додатках та інших місцях.

## Як це зробити:
```C#
DateTime dateTime = new DateTime(2020, 5, 9);
string dateString = dateTime.ToString("dd.MM.yyyy");
Console.WriteLine(dateString);
```
Введений рядок коду конвертує DateTime об'єкт в рядок ("09.05.2020") і виводить його в консоль. 

## Поглиблений аналіз
1. **Історичний контекст**: Від самого початку розвитку програмування було необхідно вирішити проблему представлення дати доступним та зручним способом для людей. C# з самого початку мав кілька методів для цього, включаючи ToString().
2. **Альтернативи**: Поряд з ToString(), використовується метод String.Format(). Нижче наведено приклад використання цього методу:
    ```C#
    DateTime dateTime = new DateTime(2020, 5, 9);
    string dateString = String.Format("{0:dd.MM.yyyy}", dateTime);
    ```
3. **Деталі реалізації**: ToString("dd.MM.yyyy") використовує форматний рядок, який визначає, як дата має бути представлена. "dd" означає день, "MM" - місяць, а "yyyy" - рік. Ви можете змінити ці параметри, щоб отримати потрібний вам формат. 

## Дивитись також
2. [Офіційна документація Microsoft з перетворення дати в рядок](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)