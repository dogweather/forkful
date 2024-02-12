---
title:                "שרשור מחרוזות"
aliases:
- he/c-sharp/concatenating-strings.md
date:                  2024-01-20T17:34:56.664944-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/concatenating-strings.md"
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
