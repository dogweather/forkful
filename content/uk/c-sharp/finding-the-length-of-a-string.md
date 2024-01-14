---
title:                "C#: Знаходження довжини рядка"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

Тривалість рядка - важлива здібність для будь-якого програміста, оскільки це дозволяє отримати інформацію про даний рядок і працювати з ним.

## Як

```C#
string str = "Привіт, світе!";

// Використання вбудованого методу Length, щоб знайти довжину рядка
int lenght = str.Length;

Console.WriteLine(lenght); // Виводиться: 14
```

У цьому прикладі ми використовуємо вбудований метод `Length`, щоб отримати довжину рядка `str`. Результатом буде ціле число, яке відображає кількість символів у рядку.

## Prof deep - Вивчення глибокого рівня

Фактично, коли ми використовуємо метод `Length` для рядка, ми отримуємо доступ до властивості `Length` об'єкту `String`. І ця властивість повертає кількість символів у рядку. 

Однак, варто зазначити, що коди символів можуть бути різними в залежності від мови. Наприклад, українські літери будуть мати різні коди, ніж англійські літери. Тому, при використанні методу `Length` для рядка, програма буде враховувати цей факт і повернуть правильну довжину рядка, враховуючи кожен символ окремо.

## Дивись також

- [Microsoft документація на метод Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1)
- [Детальний огляд властивості Length](https://www.c-sharpcorner.com/UploadFile/8ef97c/detailed-overview-of-length-properties-in-C-Sharp/)
- [Рядки в C#](https://www.w3schools.com/cs/cs_strings.php)