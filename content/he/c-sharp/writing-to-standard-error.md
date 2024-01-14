---
title:                "C#: כתיבה לתקליטור הסטנדרטי"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# למה

כאשר אנו מתכנתים, נתקל בפעמים בהם נרצה להציג בעיתון שגיאה עבור המשתמש הסופי. השתמשות בפונקציית כתיבה לעיתון שגיאה (standard error) מאפשרת לנו להדפיס הודעה למשתמש בכדי להציג לו שגיאה או אזהרה באופן ברור ומובן.

# איך לעשות זאת

לכתוב לעיתון שגיאה בשפת סי שארפ (C#) פשוט ומהיר. השתמש בפקודת Console.Error.WriteLine וכתוב את הודעת השגיאה שברצונך להציג למשתמש הסופי. לדוגמה:

```C#
Console.Error.WriteLine("שגיאה: לא ניתן לפתוח את הקובץ המבוקש.");
```

תוצאה:

שגיאה: לא ניתן לפתוח את הקובץ המבוקש.

ניתן לשלב את הפקודה הנ"ל עם תנאי if כדי להדפיס הודעת שגיאה במקרה שהתנאי לא מתקיים. לדוגמה:

```C#
if (fileNotFound)
{
    Console.Error.WriteLine("שגיאה: לא ניתן למצוא את הקובץ המבוקש.");
}
```

תוצאה:

שגיאה: לא ניתן למצוא את הקובץ המבוקש.

# חקירה מעמיקה

כאשר מדובר בכתיבה לעיתון שגיאה, חשוב לציין שההודעה תופיע בעיתון רק אם האפליקציה המובילה את הפונקציה נכשלה או הופסקה בגלל שגיאה. לכן, חשוב לטפל בכל השגיאות האפשריות ולהדגיש את השגיאה הנכונה למקרה הספציפי. כמו כן, ניתן להשתמש בפקודת Console.Error.WriteLine בשני מקומות באותו התוכנית להדגיש שגיאות שונות במקומות שונים בתוכנית.

# ראה גם

- [Console Class (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/api/system.console?view=netcore-3.1)
- [Exception Handling (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/exceptions/)