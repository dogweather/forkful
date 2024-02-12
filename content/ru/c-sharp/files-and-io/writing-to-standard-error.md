---
title:                "Запись в стандартный поток ошибок"
aliases:
- /ru/c-sharp/writing-to-standard-error.md
date:                  2024-01-29T00:05:54.775306-07:00
model:                 gpt-4-0125-preview
simple_title:         "Запись в стандартный поток ошибок"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Запись в стандартный поток ошибок (stderr) означает отправку ваших сообщений об ошибках отдельно от обычного вывода (stdout). Программисты делают это для разделения обычных данных от информации об ошибках, что помогает в ведении журналов и отладке.

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
