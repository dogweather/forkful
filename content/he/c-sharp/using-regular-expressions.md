---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
השימוש בביטויים רגולריים מאפשר לנו להתאים ולטפל במחרוזות טקסט בצורה חזקה וגמישה. מתכנתים משתמשים בזה בשביל לחפש, למצוא ולשנות טקסט לפי תבניות מורכבות בקלות רבה.

## איך לעשות:
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string text = "לדוגמה: 03-1234567 וגם 02-7654321 הם מספרים של טלפונים.";
        string pattern = @"\b\d{2}-\d{7}\b";
        
        MatchCollection matches = Regex.Matches(text, pattern);

        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```
פלט דוגמה:
```
03-1234567
02-7654321
```

## צלילה עמוקה:
ביטויים רגולריים (Regular Expressions) החלו להישתמש כבר בשנות ה-50 והיו חלק מתכנון שפות תכנות. כיום, הם חלק בלתי נפרד מרוב הטכנולוגיות. חלופות לביטויים רגולריים כוללות פונקציות של חיפוש והחלפה בלי פטרנים, אבל הם לא באותו רמת גמישות. כאשר משתמשים ב-C#, המנוע של הביטויים רגולריים כולל אופטימיזציות ותכונות משופרות שמיושמות על ידי ספריית ה-.NET Framework או .NET Core.

## ראה גם:
- [מדריך רשמי של Microsoft לביטויים רגולריים ב-C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex101 - כלי ליצירה ובדיקת ביטויים רגולריים אונליין](https://regex101.com/)
- [Regular-Expressions.info - מדריך עשיר לביטויים רגולריים](https://www.regular-expressions.info/)
