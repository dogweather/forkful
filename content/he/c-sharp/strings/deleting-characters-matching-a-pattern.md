---
date: 2024-01-20 17:42:16.535921-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05D4\
  \u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\
  \u05E0\u05D5 \u05E0\u05E4\u05D8\u05E8\u05D9\u05DD \u05DE\u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05DB\u05DC\u05DC\u05D9\u05DD\
  \ \u05DE\u05D5\u05D2\u05D3\u05E8\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\
  \u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D4\u05E1\
  \u05D9\u05E8 \u05EA\u05D5\u05D5\u05D9\u05DD \u05DC\u05D0\u2026"
lastmod: '2024-03-13T22:44:39.316445-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA \u05D4\u05D9\
  \u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\
  \u05D5 \u05E0\u05E4\u05D8\u05E8\u05D9\u05DD \u05DE\u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05DE\u05E1\u05D5\u05D9\u05DE\u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05DB\u05DC\u05DC\u05D9\u05DD \u05DE\
  \u05D5\u05D2\u05D3\u05E8\u05D9\u05DD."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

## איך לעשות:
נדגים עם C# זה פשוט. נעזר ב-`Regex.Replace`:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string pattern = @"\d"; // דוגמה לתבנית: כל ספרה
        string input = "Hello 123 World!";
        string output = Regex.Replace(input, pattern, "");
        
        Console.WriteLine(output); // פלט: Hello  World!
    }
}
```

זהו, המספרים נעלמו מהמחרוזת.

## צלילה לעומק:
מה ההקשר ההיסטורי? במה השתמשנו לפני `Regex`? ובכן, קודם לכן היה צריך ללכת תו תו ולבדוק התאמות. זה עדיין אפשרי עם לולאות ומתודות כמו `String.IndexOf`.

ראי אלטרנטיבות: `String.Replace` יכול להחליף מחרוזות קבועות ללא צורך בביטויים רגולריים. אם התבנית פשוטה, זה יעבוד מהר יותר.

עוד על פרטי היישום: `Regex.Replace` עצמו הוא כלי חזק שנבנה על ביטויים רגולריים - מערכת לתאר תבניות טקסט. הוא יכול להתמודד עם מגוון רחב של תבניות, מפשוטות עד מורכבות ביותר.

## קישורים נוספים:
- [תיעוד למחלקת Regex ב-C#](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
- [מדריך לביטויים רגולריים](https://www.regular-expressions.info/)
- [הבדלים בין Replace ל-Regex.Replace](https://stackoverflow.com/questions/4412556/whats-the-difference-between-string-replace-and-regex-replace)
