---
title:                "מחיקת תווים התואמים לתבנית"
aliases:
- /he/c-sharp/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:16.535921-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים התואמים לתבנית היא תהליך שבו אנחנו נפטרים מתווים מסוימים במחרוזת בהתאם לכללים מוגדרים. תכנתים עושים את זה כדי לנקות נתונים, להסיר תווים לא רצויים או להכין מחרוזות לעיבוד נוסף.

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
