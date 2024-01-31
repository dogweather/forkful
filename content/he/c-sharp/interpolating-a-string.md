---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:50:46.693754-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/interpolating-a-string.md"
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
