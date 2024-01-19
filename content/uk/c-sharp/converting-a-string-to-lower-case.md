---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?
Перетворення рядка в нижній регістр у C# - це процес переміщення всіх символів рядка в їхні еквівалентні символи нижнього регістру. Це корисно, коли вам потрібно зробити порівняння рядків нечутливими до регістру або коли потрібно унормоване відображення даних.

## Як це працює:
```C#
string str = "Hello, World!";
string lowerStr = str.ToLower();
Console.WriteLine(lowerStr);
```
Вивід:
```
hello, world!
```
В цій маленькій програмі `ToLower()` метод класу `System.String` перетворює рядок `"Hello, World!"` в рядок `"hello, world!"`.

## Поглиблений огляд
- **Історичний контекст**: Метод `ToLower()` був наявний в .NET Framework 1.0, що було видано у 2002 році, і продовжує залишатися в поточних версіях C#.
- **Альтернативи**: В класі `System.String` C# має є альтернативний метод `ToLowerInvariant()`, який використовується для культуронезалежних операцій.
- **Деталі реалізації**: Метод `ToLower()` в C# працює з використанням культурних налаштувань поточної системи. Це означає, якщо ваша система встановлена наприклад українською мовою, то `ToLower` буде працювати відповідно до для правил регістру української мови.

## Дивіться також
1. [Документація Microsoft про ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
2. [Стаття про різницю між ToLower та ToLowerInvariant](https://stackoverflow.com/questions/6225808/tolowerinvariant-vs-tolower)
3. [Стаття на MSDN про строкові операції в .NET](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/modify-string-contents)