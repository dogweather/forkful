---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## "מה ולמה?"

החלצת תת-מחרוזת היא הליך שבו אנו מחלצים חלק ממחרוזת. מתכנתים משתמשים בה בחלק הגדול מן הדיאלוגים עם מתכנתי VB, כאשר יש צורך לקחת חלק ממחרוזת שהמתכנת קבל.

## "איך מבצעים את זה": 

קוד בשפת C#:

```C#
using System;

public class Program
{
    public static void Main()
    {
        string str = "Hello, welcome to C# programming!";
        string sub = str.Substring(7, 7);
        
        Console.WriteLine(sub);
    }
}
```

פלט לדוגמה:

```C#
welcome
```

## "צלילה עמוקה"

החלצת תת-מחרוזת היא שיטה שנעשתה בשימוש לראשונה ב-1964, כאשר פותחה שפת BASIC. ישנן שיטות חלופיות לחלצת תת-מחרוזת, כולל `"Split()"`, המשתמשת בסימן מפריד כדי לחלץ תת-מחרוזות. בהנחה שהמספרים שנשלחים לשיטה `"Substring"` חוקיים, לא יהיה שגיאה ב-IndexOutOfRangeException.

## "ראה גם"

מאמרים שקשורים:

- [מדריך למחרוזות בשפת C# באתר Microsoft](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)