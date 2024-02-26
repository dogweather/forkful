---
date: 2024-01-20 17:34:56.664944-07:00
description: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\
  \u05D1\u05E8\u05D9\u05DD \u05E2\u05DC \u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA (concatenation), \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\
  \u05EA\u05DB\u05D5\u05D5\u05E0\u05D9\u05DD \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA\
  \ \u05E9\u05D1\u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D7\u05EA \u05D2\u05D3\u05D5\u05DC\
  \u05D4 \u05D9\u05D5\u05EA\u05E8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\u2026"
lastmod: '2024-02-25T18:49:37.569138-07:00'
model: gpt-4-1106-preview
summary: "\u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\u05D1\
  \u05E8\u05D9\u05DD \u05E2\u05DC \u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA (concatenation), \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05EA\
  \u05DB\u05D5\u05D5\u05E0\u05D9\u05DD \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D7\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\
  \ \u05D9\u05D5\u05EA\u05E8. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\u2026"
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
כאשר אנחנו מדברים על שרשור מחרוזות (concatenation), אנחנו מתכוונים לתהליך שבו מחברים שתי מחרוזות או יותר למחרוזת אחת גדולה יותר. תכניתנים עושים זאת כדי לבנות מחרוזות מתוך חתיכות טקסט, כמו כתובות או הודעות למשתמש.

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
