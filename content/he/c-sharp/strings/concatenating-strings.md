---
date: 2024-01-20 17:34:56.664944-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D6\u05D4 \u05E4\
  \u05E9\u05D5\u05D8. \u05E7\u05D7\u05D5 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\
  \u05D7\u05EA, \u05D4\u05D5\u05E1\u05D9\u05E4\u05D5 \u05E2\u05D5\u05D3 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05E2\u05DD `+` \u05D0\u05D5 \u05E2\u05DD \u05DE\u05EA\
  \u05D5\u05D3\u05EA `string.Concat()`. \u05D4\u05E0\u05D4 \u05DB\u05DE\u05D4 \u05D3\
  \u05D5\u05D2\u05DE\u05D0\u05D5\u05EA."
lastmod: '2024-03-13T22:44:39.329179-06:00'
model: gpt-4-1106-preview
summary: "\u05D6\u05D4 \u05E4\u05E9\u05D5\u05D8."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## איך לעשות:
זה פשוט. קחו מחרוזת אחת, הוסיפו עוד מחרוזת עם `+` או עם מתודת `string.Concat()`. הנה כמה דוגמאות:

```C#
string hello = "שלום";
string world = "עולם";
string combined = hello + " " + world;
Console.WriteLine(combined); // יוצא "שלום עולם"

// דוגמה נוספת
string firstName = "יונתן";
string lastName = "כהן";
string fullName = string.Concat(firstName, " ", lastName);
Console.WriteLine(fullName); // יוצא "יונתן כהן"
```

עבור בניית מחרוזות גדולות או פעולת שרשור רבה, כדאי לשקול שימוש במחלקת `StringBuilder`:

```C#
StringBuilder sb = new StringBuilder();
sb.Append("היי,");
sb.Append(" כיצד");
sb.Append(" הולך?");
Console.WriteLine(sb.ToString()); // הדפסה של "היי, כיצד הולך?"
```

## צלילה לעומק
השרשור של מחרוזות הוא מושג יסודי בעולם התכנות, שהתפתח מאז שהמחשבים הראשונים התחילו לעבד טקסט. ב-C#, השימוש ב`+` הוא נוח, אבל לא תמיד יעיל, כי כל שימוש בו יוצר מחרוזת חדשה. עם זאת, `StringBuilder` מיועד לשימוש כאשר יש צורך לשנות או להרכיב מחרוזות רבות ביעילות.
ב-C# 6 ומעלה, אפשר להשתמש גם ב-interpolation של מחרוזות, שהוא דרך קריאה ויעילה לשלב טקסט ומשתנים:

```C#
int age = 30;
string name = "דניאל";
string greeting = $"שמי {name} ואני בן {age} שנים";
Console.WriteLine(greeting);
```

## ראו גם
- מדריך על `StringBuilder`: https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netframework-4.8
- מדריך על interpolation של מחרוזות: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated
- מאמר על יעילות השרשור של מחרוזות: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#string-concatenation
