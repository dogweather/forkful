---
title:                "חילוץ תת-מחרוזות"
aliases:
- /he/c-sharp/extracting-substrings/
date:                  2024-01-20T17:45:16.099119-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה חלק מחרוזת ולמה לעשות את זה? להוציא חלק ממחרוזת זה לקחת חתיכה מתוך מחרוזת אחת וליצור ממנה מחרוזת חדשה. תוכניתנים עושים זאת כדי לעבד נתונים, לסנן תוכן או להציג חלק מפרטים ממחרוזת ארוכה.

## How to:
הנה דוגמה לאיך עושים זאת ב-C#:

```C#
using System;

class Program {
    static void Main() {
        string fullText = "שלום, עולם של תוכנות!";
        string extracted = fullText.Substring(6, 5); // קח את התווים החל מהאינדקס ה-6, באורך 5 תווים
        Console.WriteLine(extracted); // ידפיס "עולם"
    }
}
```
פלט הדוגמה:
```
עולם
```

## Deep Dive:
לשלוף חלק ממחרוזת היא טכניקה עתיקה. מראשית ימי המחשב, כאשר היה צורך לטפל במונחים טקסטואליים. ב-C#, למחלקת `String` יש כמה שיטות לעשות את זה, כמו `Substring`, `Split`, `Remove`, ועוד. בחרו בעיקר ב-`Substring` כאשר אתם יודעים בדיוק את המיקום ואת אורך החלק שאתם צריכים. זכרו, אינדקסים ב-C# מתחילים ב-0.

## See Also:
בדקו את המקורות הבאים למידע נוסף:
- [Microsoft Docs - String.Substring Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [Microsoft Docs - String Methods in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-5.0)
- [Stack Overflow - Extracting a substring in C#](https://stackoverflow.com/questions/218384/what-is-a-nullreferenceexception-and-how-do-i-fix-it)
