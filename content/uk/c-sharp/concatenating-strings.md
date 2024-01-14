---
title:                "C#: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Є багато причин, чому в програмуванні часто потрібно з'єднувати рядки. Наприклад, для створення повідомлення користувачу, форматування виводу даних або для створення адресних рядків для веб-адрес.

## Як

Для з'єднання рядків у C#, використовуйте оператор "+" або метод Concat (). Наприклад:

```C#
string name = "Олена";
string greeting = "Привіт " + name + "!";
Console.WriteLine(greeting);
```
Вивід: Привіт Олена!

Також можна використовувати $ перед початком рядка, щоб створити шаблонні рядки, які дозволяють вставляти значення змінних без необхідності використовувати оператор "+" або метод Concat (). Наприклад:

```C#
string name = "Олена";
string greeting = $"Привіт {name}!";
Console.WriteLine(greeting);
```
Вивід: Привіт Олена!

## Deep Dive

При з'єднанні рядків у C#, важливо розуміти, що це дійсно робить. Коли використовується оператор "+", змінні рядків конкатенуються в новий рядок. Однак, якщо цей процес використовується багато разів, може призвести до зайвого навантаження на пам'ять. Тому варто використовувати метод Concat (), який створює новий об'єкт рядка без зайвого навантаження пам'яті.

## See Also

1. [Оператор + у C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/addition-operator)
2. [Метод Concat () у C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat)
3. [Використання $ в шаблонних рядках у C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)