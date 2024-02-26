---
date: 2024-01-20 17:45:16.099119-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05DC\u05E7 \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D5\u05DC\u05DE\u05D4 \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA\
  \ \u05D6\u05D4? \u05DC\u05D4\u05D5\u05E6\u05D9\u05D0 \u05D7\u05DC\u05E7 \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05DC\u05E7\u05D7\u05EA \u05D7\u05EA\
  \u05D9\u05DB\u05D4 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D0\u05D7\u05EA \u05D5\u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05DE\u05E0\u05D4\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D7\u05D3\u05E9\u05D4. \u05EA\u05D5\u05DB\
  \u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05DC\u05E1\u05E0\u05DF\u2026"
lastmod: '2024-02-25T18:49:37.564494-07:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05DC\u05E7 \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05D5\u05DC\u05DE\u05D4 \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D6\
  \u05D4? \u05DC\u05D4\u05D5\u05E6\u05D9\u05D0 \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05DC\u05E7\u05D7\u05EA \u05D7\u05EA\u05D9\
  \u05DB\u05D4 \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\
  \u05D7\u05EA \u05D5\u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05DE\u05E0\u05D4 \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D7\u05D3\u05E9\u05D4. \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05DC\u05E1\u05E0\u05DF\u2026"
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
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
