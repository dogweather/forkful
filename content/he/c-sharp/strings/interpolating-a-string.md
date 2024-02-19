---
aliases:
- /he/c-sharp/interpolating-a-string/
date: 2024-01-20 17:50:46.693754-07:00
description: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05D8\u05DB\
  \u05E0\u05D9\u05E7\u05D4 \u05D1-C# \u05DC\u05E6\u05D9\u05E8\u05D5\u05E3 \u05DE\u05E9\
  \u05EA\u05E0\u05D9\u05DD \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05E7\u05D5\u05D3 \u05D5\u05DC\u05E9\u05E4\
  \u05E8 \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D1\u05E2\u05EA \u05D9\u05E6\u05D9\
  \u05E8\u05EA \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\u05D9\
  \u05D9\u05DD."
lastmod: 2024-02-18 23:08:52.827226
model: gpt-4-1106-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05D8\u05DB\
  \u05E0\u05D9\u05E7\u05D4 \u05D1-C# \u05DC\u05E6\u05D9\u05E8\u05D5\u05E3 \u05DE\u05E9\
  \u05EA\u05E0\u05D9\u05DD \u05D5\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05D1\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4 \u05DB\
  \u05D3\u05D9 \u05DC\u05E4\u05E9\u05D8 \u05E7\u05D5\u05D3 \u05D5\u05DC\u05E9\u05E4\
  \u05E8 \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA \u05D1\u05E2\u05EA \u05D9\u05E6\u05D9\
  \u05E8\u05EA \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\u05D9\
  \u05D9\u05DD."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזות היא טכניקה ב-C# לצירוף משתנים וביטויים בתוך מחרוזת. מתכנתים משתמשים בה כדי לפשט קוד ולשפר קריאות בעת יצירת טקסטים דינמיים.

## איך לעשות:
ב-C#, באמצעות אינטרפולציה של מחרוזות אפשר לכלול ביטויים בתוך מחרוזות באופן ישיר. תראו כאן דוגמה פשוטה:

```C#
string name = "דורון";
int age = 30;
string greeting = $"שלום, שמי הוא {name} ואני בן {age}.";
Console.WriteLine(greeting);
```

תוצאה:
```
שלום, שמי הוא דורון ואני בן 30.
```

## עיון מעמיק
טכניקת אינטרפולציה נתמכת מגרסת C# 6.0 והיא מחליפה את שיטת `String.Format`. היתרון על פני גישות קודמות הוא שהקוד הרבה יותר קריא ופחות פרונה לשגיאות – אין צורך בספירת פלייסהולדרים ומיפויים ידני. מאחורי הקלעים, המהדר של C# מפרק את הביטוי לעצמים של `String.Format`.

האלטרנטיבות לאינטרפולציה כוללות שימוש ב- `+` לשרשור מחרוזות (לא מומלץ בקוד אמיתי מסיבות ביצועים) ו`StringBuilder` למקרים שבהם יש הרבה שינויים דינמיים.

## ראה גם
- [Microsoft's official documentation on string interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [C# 6.0 in a Nutshell - כל העניין של C# 6.0](http://www.albahari.com/nutshell/)
- [String.Format vs. String Interpolation](https://stackoverflow.com/questions/33140154/string-format-vs-string-interpolation-performance) - דיון ב-StackOverflow על ביצועים של String.Format לעומת אינטרפולציה
