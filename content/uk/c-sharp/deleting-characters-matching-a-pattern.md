---
title:                "C#: Видалення символів, які відповідають шаблону"
simple_title:         "Видалення символів, які відповідають шаблону"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### Чому

Чи часто ви знаходите себе в ситуації, коли необхідно видалити певні символи із тексту? Це може бути знайомо багатьом програмістам. У цій статті ми розглянемо один із способів видалення символів у C#.

### Як

У C# існує кілька методів для видалення символів з тексту. Один із них - використання регулярних виразів (Regex). Наприклад, якщо ви хочете видалити всі цифри зі строки, ви можете використати такий код:

```C#
using System;
using System.Text.RegularExpressions;

string text = "Hello123World";
string pattern = @"\d"; // шаблон для пошуку цифр
string result = Regex.Replace(text, pattern, ""); // видалення символів, які відповідають шаблону

Console.WriteLine(result); // виведе "HelloWorld"
```

У цьому прикладі ми використали метод Regex.Replace(), який приймає три аргументи: текст для заміни, шаблон для пошуку і текст, який замінить знайдені символи.

Іншим методом для видалення символів є використання методу Remove() класу StringBuilder.

```C#
using System.Text;

string text = "Hello123World";
StringBuilder sb = new StringBuilder(text);

for (int i = 0; i < sb.Length; i++)
{
    if (char.IsDigit(sb[i])) // перевірка, чи символ є цифрою
    {
        sb.Remove(i, 1); // видалення символу
        i--; // зменшення індексу, оскільки довжина строки змінилась
    }
}

Console.WriteLine(sb.ToString()); // виведе "HelloWorld"
```

Існує багато інших методів для видалення символів, тож вам залишається обрати той, який найбільше підходить вашим потребам.

### Deep Dive

У більшості випадків це завдання видалення символів є частиною більшої задачі, наприклад, очистка даних для обробки або форматування введеного користувачем тексту. Однак, це також може бути корисним при роботі з певними видами даних, де потрібно відокремити числову частину від тексту, наприклад, при роботі з CSV файлами.

Цей метод видалення символів також може бути корисним при пошуку і фільтрації даних за певними критеріями.

### Дивіться також

- [Регулярні вирази у C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Метод Replace() класу String](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [Метод Remove() класу StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder.remove)