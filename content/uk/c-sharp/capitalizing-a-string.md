---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Коли ми кажемо про великі літери у рядку, ми маємо на увазі перетворення першої букви кожного слова на велику. Програмісти використовують цей прийом для форматування тексту, наприклад, для заголовків або імен.

## Як це зробити:
```C#
using System;
using System.Globalization;

public class Capitalizer
{
    public static void Main()
    {
        string phrase = "привіт, як справи?";
        string capitalizedPhrase = CapitalizeEachWord(phrase);
        
        Console.WriteLine(capitalizedPhrase); // Вивід: "Привіт, Як Справи?"
    }
    
    private static string CapitalizeEachWord(string text)
    {
        TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
        return textInfo.ToTitleCase(text);
    }
}
```

## Поглиблений аналіз:
Для великих літер у C# є кілька підходів. В прикладі вище використано `TextInfo.ToTitleCase`, який є частиною `CultureInfo`. Це особливо корисно, оскільки поважає локалізацію. Метод `ToTitleCase` не змінює слова, які вже починаються з великої літери, що може бути як плюс, так і мінус, залежно від ситуації. Альтернативний метод – використання `ToLower()` або `ToUpper()` для перетворення на малий або великий регістри відповідно. Ці методи не розрізняють слова, тому результат може бути не завжди той, який ви очікуєте.

## Додатково:
- Документація Microsoft щодо `TextInfo.ToTitleCase`: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase
- Стаття про різні способи форматування строк у C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings 
- Вікіпедія про нотацію у програмуванні: https://en.wikipedia.org/wiki/Letter_case#Title_case
