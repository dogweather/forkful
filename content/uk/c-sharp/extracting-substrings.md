---
title:                "«Видобування підрядків»"
html_title:           "C#: «Видобування підрядків»"
simple_title:         "«Видобування підрядків»"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і чому?
Вилучення підрядків є важливою технікою в програмуванні, яка дозволяє виділити окрему частину рядка для подальшого використання або обробки. Програмісти часто використовують цей підхід, щоб оптимізувати свій код та полегшити роботу з текстовими даними.

## Як це зробити:
Для виконання вилучення підрядків у C# ми можемо скористатися методом ```Substring()```. Цей метод приймає два параметри: початкову позицію та кількість символів, які необхідно виділити. Наприклад, якщо ми маємо рядок "Hello World!", то за допомогою методу ```Substring(0, 5)``` ми отримаємо підрядок "Hello". 

```C#
string str = "Hello World!";
string substr = str.Substring(0, 5);
Console.WriteLine(substr);
// Output: Hello
```

Безпосередньо використовуючи індекси, ми можемо використовувати цей метод для отримання підрядків з будь-якої частини рядка, наприклад:

```C#
string str = "Hello World!";
string substr1 = str.Substring(0, 5);
Console.WriteLine(substr1);
// Output: Hello

string substr2 = str.Substring(4, 5);
Console.WriteLine(substr2);
// Output: o Wor

string substr3 = str.Substring(6, 5);
Console.WriteLine(substr3);
// Output: World
```

## Глибші дослідження:
Вилучення підрядків з тексту є часто використовуваною технікою, яка виникла ще у 1960-х роках. Існує також інший підхід - використання регулярних виразів. Регулярні вирази дозволяють здійснювати більш гнучке та складне вилучення підрядків з тексту.

Однак, використання методу ```Substring()``` в C# є більш оптимальним та швидшим способом для простого вилучення підрядків. Зверніть увагу, що метод вилучення підрядка в C# включає початковий індекс, але не включає кінцевий.

## Дивіться також:
Для отримання більш детальної інформації про метод ```Substring()``` та регулярні вирази, перегляньте офіційну документацію за посиланням [тут](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0) та [тут](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference?view=net-5.0).