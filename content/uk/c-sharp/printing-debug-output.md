---
title:                "Друк відлагоджувального виводу"
html_title:           "Arduino: Друк відлагоджувального виводу"
simple_title:         "Друк відлагоджувального виводу"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Друк діагностичного виводу - це процес, за якого програма виводить спеціальні повідомлення в консоль для слідкування за її роботою та виявлення помилок. Програмісти використовують його для легкого тестування й розшифрування коду.

## Як це виконати:
Отже, ось як ви можете друкувати діагностичний вивід у C#:

```C#
using System.Diagnostics;

public class Program
{
    public static void Main()
    {
        Debug.WriteLine("Це діагностичне повідомлення");
    }
}
```

При виконанні цього коду в консолі з'явиться повідомлення: `Це діагностичне повідомлення`. Саме так, ви повинні бачити свій діагностичний вивід.

## Поглиблений огляд

**Історичний контекст**: З початку розвитку комп'ютерних систем, діагностичний вивід був важливим інструментом для тестування програм і виявлення помилок.

**Альтернативи**: Інший популярний інструмент для видачі діагностичних даних - це `Trace.WriteLine()`. Він має відмінності у тому, коли і як він виводить вивід. Використовуйте його, коли вам потрібно виводити діагностичні дані як у режимі налагодження, так і у релізі.

**Деталі імплементації**: `Debug.WriteLine()` працює лише у режимі налагодження і не буде видавати жодного повідомлення, коли програма скомпільована в режимі Release.

## Дивіться також 

1. Офіційна документація Microsoft про `Debug.WriteLine()`: [посилання](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug.writeline?view=net-5.0)

2. Порівняння `Debug.WriteLine()` та `Trace.WriteLine()`: [посилання](https://stackoverflow.com/questions/29620290/what-is-the-difference-between-debug-write-and-trace-write)

3. Путівник по налагодженню C# програм: [посилання](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-absolute-beginners?view=vs-2019)