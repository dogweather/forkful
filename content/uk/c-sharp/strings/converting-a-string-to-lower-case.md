---
date: 2024-01-20 17:38:11.397890-07:00
description: "How to: | \u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438: ."
lastmod: '2024-03-13T22:44:49.268476-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

## How to: | Як це зробити:
```C#
using System;

class Program
{
    static void Main()
    {
        string original = "Привіт, Світе!";
        string lowercased = original.ToLower();

        Console.WriteLine(lowercased);  // Output: привіт, світе!
    }
}
```

## Deep Dive | Поглиблений Розгляд:
У минулому, перетворення тексту в один регістр могло бути не таким тривіальним завданням, адже стандарти кодування символів були різними. Зараз, з Unicode, C# використовує метод `ToLower()`, який враховує локальні стандарти при перетворенні.

Альтернативою `ToLower()` є `ToLowerInvariant()`. Цей метод ігнорує локалізацію і використовується, коли потрібна єдина форма для всіх користувачів, незалежно від їх мови та культури.

Зауважте, що ці методи можуть вести себе неочікувано з деякими символами, на кшталт лігатур. Розробники повинні знати особливості своєї аудиторії та дані, з якими вони працюють, щоб обрати відповідний метод.

## See Also | Дивись Також:
- [
System.String.ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [
System.String.ToLowerInvariant Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant)
- [Unicode Standard](http://www.unicode.org/standard/standard.html)
