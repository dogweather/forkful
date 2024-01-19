---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і чому?

Пошук та заміна тексту - це процес виявлення певного шаблону в тексті та заміни його на інший текст. Програмісти роблять це, щоб спростити операції зі строками та підтримати чистий код.

## Як це робити:

Пошук та заміну тексту в C# можна виконати за допомогою методу String.Replace().

```C#
string source = "Привіт, Світ!";
string toFind = "Світ";
string toReplace = "Україна";

string result = source.Replace(toFind, toReplace);
System.Console.WriteLine(result); // Outputs "Привіт, Україна!"
```

## Поглиблений погляд:

Історично, пошук та заміна тексту виявився корисним інструментом для обробки тексту в галузі програмування. Незалежно від діалекту мови, функціональність пошуку та заміни тексту завжди була основою роботи зі строками.

Як альтернатива, в C# є регулярні вирази, які можуть використовуватися для більш складних шаблонів.

Виконуючи пошук та заміну тексту, String.Replace використовує радіус пошуку, що створює нову строку, а не модифікує поточний.

## Дивись також:

1. [Довідка Microsoft по String.Replace](https://docs.microsoft.com/uk-ua/dotnet/api/system.string.replace?view=net-5.0)
2. [Довідка Microsoft по регулярних виразах в C#](https://docs.microsoft.com/uk-ua/dotnet/standard/base-types/regular-expressions)