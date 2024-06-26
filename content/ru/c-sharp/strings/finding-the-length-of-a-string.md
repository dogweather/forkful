---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:25.537266-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 C# \u0441\u0432\u043E\u0439\u0441\u0442\u0432\u043E `string.Length`\
  \ \u0434\u0430\u0435\u0442 \u0432\u0430\u043C \u043A\u043E\u043B\u0438\u0447\u0435\
  \u0441\u0442\u0432\u043E \u0441\u0438\u043C\u0432\u043E\u043B\u043E\u0432 \u0432\
  \ \u0441\u0442\u0440\u043E\u043A\u0435. \u0412\u043E\u0442 \u043A\u0430\u043A \u0435\
  \u0433\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  ."
lastmod: '2024-03-13T22:44:45.039813-06:00'
model: gpt-4-0125-preview
summary: "\u0412 C# \u0441\u0432\u043E\u0439\u0441\u0442\u0432\u043E `string.Length`\
  \ \u0434\u0430\u0435\u0442 \u0432\u0430\u043C \u043A\u043E\u043B\u0438\u0447\u0435\
  \u0441\u0442\u0432\u043E \u0441\u0438\u043C\u0432\u043E\u043B\u043E\u0432 \u0432\
  \ \u0441\u0442\u0440\u043E\u043A\u0435."
title: "\u041F\u043E\u0438\u0441\u043A \u0434\u043B\u0438\u043D\u044B \u0441\u0442\
  \u0440\u043E\u043A\u0438"
weight: 7
---

## Как это сделать:
В C# свойство `string.Length` дает вам количество символов в строке. Вот как его использовать:

```C#
using System;

class Program
{
    static void Main()
    {
        string example = "Привет, мир!";
        Console.WriteLine(example.Length); // Вывод: 13
    }
}
```

Легко, правда? Но помните, оно считает *символы*, а не байты. С эмодзи или специальными символами все может стать сложнее. Об этом позже.

## Подробнее
Исторически, нахождение длины строки было связано с управлением и манипуляцией памятью в программировании. Поскольку C# является языком высокого уровня, он абстрагирует эту низкоуровневую работу. Тем не менее, хорошо знать, что находится под капотом.

Альтернативы? Конечно! Вы можете встретить `example.ToCharArray().Length` на практике, но это просто лишняя работа для получения того же результата.

Теперь о тех сложных символах. Свойство `Length` в C# подсчитывает объекты `char` строки, каждый из которых представляет собой единицу кода UTF-16. Это нормально, пока вы не столкнетесь с *суррогатными парами* – символами вроде эмодзи, которым нужны два объекта `char`. Вот в чем дело: `Length` считает их за два. Да.

Для точного подсчета *визуальных* символов или *кластеров графем* вам понадобится класс `StringInfo` из `System.Globalization`:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string example = "👍"; // Эмодзи в виде пальца вверх

        Console.WriteLine(example.Length); // Вывод: 2 <- Из-за суррогатной пары!
        Console.WriteLine(new StringInfo(example).LengthInTextElements); // Вывод: 1
    }
}
```

Понимаете разницу? Это не просто академический интерес; это может значительно повлиять на обработку текста.

## Смотрите также
Изучите больше с этими ресурсами:

- [Официальная документация Microsoft по строкам](https://docs.microsoft.com/ru-ru/dotnet/csharp/programming-guide/strings/)
- [Понимание Unicode и UTF-16](https://unicodebook.readthedocs.io/unicode_encodings.html)
- [Документация по классу StringInfo](https://docs.microsoft.com/ru-ru/dotnet/api/system.globalization.stringinfo?view=net-6.0)

Знайте свои строки, обращайтесь с ними мудро и пишите код, который считается – во всех смыслах.
