---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:54.775306-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 C#, \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0438\
  \ \u0432 stderr \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435\
  \ `Console.Error.WriteLine()`. \u042D\u0442\u043E \u043F\u043E\u0445\u043E\u0436\
  \u0435 \u043D\u0430 `Console.WriteLine()`, \u0442\u043E\u043B\u044C\u043A\u043E\
  \ \u043D\u0430\u0446\u0435\u043B\u0435\u043D\u043E \u043D\u0430 \u043F\u043E\u0442\
  \u043E\u043A \u043E\u0448\u0438\u0431\u043E\u043A."
lastmod: '2024-03-13T22:44:45.086651-06:00'
model: gpt-4-0125-preview
summary: "\u0412 C#, \u0434\u043B\u044F \u0437\u0430\u043F\u0438\u0441\u0438 \u0432\
  \ stderr \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 `Console.Error.WriteLine()`."
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
weight: 25
---

## Как это сделать:
В C#, для записи в stderr используйте `Console.Error.WriteLine()`. Это похоже на `Console.WriteLine()`, только нацелено на поток ошибок.

```C#
using System;

class Program
{
    static void Main()
    {
        Console.WriteLine("Сообщение стандартного вывода."); // Переходит в stdout
        Console.Error.WriteLine("Сообщение об ошибке!"); // Переходит в stderr
    }
}
```

Пример вывода, когда всё хорошо:

```
Сообщение стандартного вывода.
```

Но, если что-то не так, вы увидите:

```
Сообщение стандартного вывода.
Сообщение об ошибке!
```

Сообщение об ошибке появляется в консоли или может быть перенаправлено в файл.

## Подробнее
Исторически, разделение stdout и stderr восходит к системам Unix, где это позволяло чистую обработку данных и обработку ошибок. В C# (и .NET в целом), `Console.Out` представляет stdout, в то время как `Console.Error` представляет stderr.

Вы можете перенаправлять оба потока, используя `Console.SetOut()` и `Console.SetError()`. Потоки, такие как `FileStream` или `StringWriter`, могут перехватывать вывод для ведения журнала. Это критически важно в сценариях, когда сообщения об ошибках не должны смешиваться с обычными данными, например, когда stdout направляется в другую программу.

## Смотрите также
- [Свойство Console.Error - Документация Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [Класс потока .NET - Документация Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.io.stream)
