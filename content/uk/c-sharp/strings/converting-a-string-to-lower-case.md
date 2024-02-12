---
title:                "Перетворення рядка у нижній регістр"
aliases: - /uk/c-sharp/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:11.397890-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? | Що і Чому?
Приведення рядків до нижнього регістру - це процес, коли всі великі літери у рядку замінюються на малі. Роблять це для уніфікації даних, наприклад, при порівнянні тексту без врахування регістру.

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
