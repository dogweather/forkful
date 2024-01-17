---
title:                "כתיבת קובץ טקסט"
html_title:           "C#: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא תהליך שבו משתמשים בתוכנת קוד ליצירת קובץ שמכיל טקסט פשוט, כגון מסמכי טקסט וקבצי תרגום. תהליך זה נחשב לחלק חשוב מאוד בתכנות ומשמש לשמירת מידע ונתונים שונים.

## איך לעשות:
```
using System.IO;
File.WriteAllText("myfile.txt", "This is a sample text file.");
```

הקוד לעיל מייצר קובץ טקסט חדש בשם "myfile.txt" ומכיל את המשפט "This is a sample text file." ישנן גם אפשרויות אחרות לכתיבת קבצים טקסט, כגון כתיבה שורה אחרי שורה, או כותב נתונים לתוך קובץ קיים.

## עומק הטלה:
כתיבת קבצים נעשה כבר שנים רבות, ולמרבה המזל מתברר כי קבצים טקסט הם כלי נוח ויעיל לשמירת מידע ונתונים. יתר על כן, קיימות גם אפשרויות חדשות כגון שימוש בבסיסי הנתונים כדי לשמור נתונים מבוקשים.

## לצפות גם:
למידע נוסף על כתיבת קבצים ב-C# ניתן לבקר באתר המתכנתים הרשמי של מייקרוסופט: https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writelines?view=net-5.0