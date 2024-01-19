---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

באינטרפולציה של מחרוזות הוא תהליך בו אנחנו משניסים ביטויים בתוך מחרוזת ממש, במקום להשתמש ב-
concatenation. מתכנתים משתמשים בזה לחסכון בקוי קוד, קריאות יותר, וקלות בתחזוקה. 

## איך לעשות:

נפריט שני דוגמאות של C# עם פלט, שמשתמשות באינטרפולציה של מחרוזות:

```C#
string name = "Yoni";
int age = 30;
Console.WriteLine($"שלום, השם שלי הוא {name} ואני בן {age}.");
```
פלט: ``שלום, השם שלי הוא Yoni ואני בן 30.``

או כאן שימוש בביטויים מורכבים יותר:

```C#
int x = 5;
int y = 10;
Console.WriteLine($"הסכום של {x} פלוס {y} שווה: {x+y}");
```
פלט: ``הסכום של 5 פלוס 10 שווה: 15``

## צלילה עמוקה

מבחינה היסטורית, האינטרפולציה נוספה רק ב-C# 6.0. לפני זה, הייתה הצורך בשילוב מורכב של רצפים ופורמטים. אלטרנטיבה לאינטרפולציה היא באמצעות שיטת Format של הקלאס String, אך האינטרפולציה התפרצה כדרך העדיפה בשל קריאות ונוחות גבוהה יותר.

באופן תחתון, ה-C# מחדיר את המחרוזת המשולבת כשימוש ב-String.Format(), כך שלא נגרמים פגמים קצויים מעבר לביצועים נמוכים.


## ראה גם:

1. [C# String Interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
2. [The evolution of C#](https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history)
3. [Effective C# String and I/O Operations](https://www.pluralsight.com/courses/csharp-effective-string-io)

תודה על קריאת המאמר!