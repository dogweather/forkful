---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

```Ukrainian```
## Що це і навіщо?
Витягування підрядків - це процес отримання частини рядка від початкового до кінцевого індексу. Програмісти використовують це для обробки даних і маніпуляцій з текстом.

## Як це зробити:
```C#
string sampleStr = "Вітаємо в C#";
string substring = sampleStr.Substring(8);
Console.WriteLine(substring);  // Виводимо: C#
```
Тут ми витягли підрядок, починаючи з 8-го символу у рядку.

```C#
string substringWithLength = sampleStr.Substring(0, 7);
Console.WriteLine(substringWithLength);  // Виводимо: Вітаємо
```
В цьому прикладі ми витягли підрядок довжиною в 7 символів, починаючи з першого символу.

## Поглиблене вивчення:
Витягування підрядків існує з перших версій С. Метод `Substring` був покращений в C# для обробки негативних індексів і невдовзі символів. З C# 8.0 ви можете безпечно використовувати "slicing" навіть для нульових рядків. 

```C#
string safeSubstring = sampleStr[8..];  // Ту ж саму дію можна зробити за допомогою "slicing"
```
Проте, якщо ви все ж таки потребуєте використовувати більш старі методи витягування підрядків, обидва зазначені раніше підходи будуть працювати і в нинішній версії C#.

## Дивіться також:
[Документація Microsoft про String.Substring](https://docs.microsoft.com/uk-ua/dotnet/api/system.string.substring)