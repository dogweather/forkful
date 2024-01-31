---
title:                "Запис в стандартний потік помилок"
date:                  2024-01-19
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"

category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Стандартний потік помилок (stderr) - це окремий від потоку виводу канал для повідомлень про помилки та діагностики. Програмісти використовують його, щоб розділити звичайний вивід програми і повідомлення про помилки, що дозволяє легко перехоплювати та обробляти помилки.

## How to:
```C#
using System;

class Program
{
    static void Main()
    {
        Console.Error.WriteLine("Це повідомлення про помилку");
        Console.WriteLine("Це стандартний вивід програми");
    }
}
```
Імітація запуску у терміналі та приклад виведення:
```plaintext
Це стандартний вивід програми
Це повідомлення про помилку
```

## Deep Dive
В Unix-подібних системах, stderr було впроваджено для відділення звичайного виводу від повідомлень про помилки, щоб можна було перенаправити їх у різні місця. У C#, `Console.Error` є аналогом стандартного потоку помилок, його також можна перенаправити використовуючи `Console.SetError()`. Альтернативи включають логування за допомогою бібліотек як NLog або log4net.

## See Also
- [Console.Error Property (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.console.error)
- [NLog – Advanced .NET Logging](https://nlog-project.org/)
- [log4net – Logging Framework](https://logging.apache.org/log4net/)
