---
title:                "C#: З'єднання рядків"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви програмуєте на C#, то скоріш за все ви стикалися з необхідністю об'єднати декілька рядків в один. Ця операція називається конкатенуванням рядків і є доволі поширеною в програмуванні. 

## Як це зробити

У C# для конкатенування рядків використовується оператор "+" або метод String.Concat. Давайте подивимось на наступний приклад:

```C#
string firstName = "Андрій";
string lastName = "Коваль";
string fullName = firstName + " " + lastName;

Console.WriteLine(fullName);
```
Вивід: "Андрій Коваль"

Також можна використовувати метод String.Format, щоб об'єднати рядки під час форматування вихідного рядка:

```C#
string firstName = "Андрій";
string lastName = "Коваль";
string fullName = string.Format("{0} {1}", firstName, lastName);

Console.WriteLine(fullName);
```
Вивід: "Андрій Коваль"

Крім того, можна використовувати знак "?" або метод String.Join, якщо ви маєте більше двох рядків, які потрібно об'єднати.

## Глибоке заглиблення

При використанні оператора "+" або String.Concat, Компілятор C# автоматично використовує метод StringBuilder для об'єднання рядків. Це дозволяє зменшити кількість створених об’єктів і оптимізує продуктивність програми.

Також важливо врахувати, що рядки в C# є незмінними об'єктами, тому кожен раз, коли ви змінюєте рядок, створюється новий об'єкт в пам'яті. Тому при конкатенуванні великих кількостей рядків, краще використовувати метод StringBuilder замість "+" або String.Concat.

## Дивись також

- [Офіційна документація C#](https://docs.microsoft.com/uk-ua/dotnet/csharp/)
- [Стаття на тему "Робота з рядками в C#"](https://coderoad.ru/494342/%D0%A0%D0%B0%D0%B1%D0%BE%D1%82%D0%B0-%D1%81-%D1%80%D1%8F%D0%B4%D0%BA%D0%B0%D0%BC%D0%B8-%D0%B2-C)
- [Інструкція по використанню методу String.Format](https://metanit.com/sharp/tutorial/12.3.php)