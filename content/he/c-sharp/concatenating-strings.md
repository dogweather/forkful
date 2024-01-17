---
title:                "מציבי מחרוזות"
html_title:           "C#: מציבי מחרוזות"
simple_title:         "מציבי מחרוזות"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?

צירוף מחרוזות הוא פשוט פעולה שבה מחרוזות שונות מוחברות יחד כדי ליצור מחרוזת אחת גדולה. תכניות במחשב שכתובות בשפת C# משתמשות בפעולה זו כדי ליצור מחרוזות דינמיות, כלומר מחרוזות שיכולות להיות משתנות.

## איך לעשות?

```C#
// השתמשו בסימן פלוס (+) לשימוש כדי לצרף שתי מחרוזות יחד
string hello = "שלום";
string world = "עולם";
string helloWorld = hello + world;
// המחרוזת החדשה תהיה: "שלוםעולם"

// ניתן גם להשתמש בפעולה זו בתוך הפלטפורמה של WriteLine כדי להדפיס מחרוזת משתנה
Console.WriteLine("שלום " + "עולם"); // פלט: "שלום עולם"

// כמו כן, ניתן להשתמש בפעולה זו כדי לצרף מחרוזות לתוך מחרוזת קיימת
string text = "היי,";
text += "מה קורה?"; // text עכשיו יהיו "היי,מה קורה?"
```

## פעילות עמוקה

צירוף מחרוזות הוא טכניקה שמאוד נפוצה בתכנות וקיימת כבר עוד מימי השפה הראשונות של תכנות המחשב. בכמה שפות תכנות, כמו JavaScript ו-Python, ניתן גם להשתמש בסימון שבשפת C# (הוספת שני מחרוזות יחד) כדי לצרף מחרוזות.

במקום להשתמש בפעולה של צירוף מחרוזות, אפשר גם להשתמש באובייקטים שמיועדים לניהול מחרוזות ומספקים כמה פעולות שימושיות כמו חיתוך וחיפוש. אנחנו גם יכולים למצוא את הפעולה הזו במספר פלטפורמות של מיקרוסופט, כמו ספריות .NET ו-Windows.

## ראו גם

- [מדריך לויזואל סטודיו קוד בינוני: צירוף מחרוזות](https://code.visualstudio.com/docs/editor/codebasics#_concatenate-strings)
- [מדריך רשמי של C# על תכנות במחרוזות](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/#concat)
- [מדריך רשמי של C# על אובייקטי מחרוזת](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)