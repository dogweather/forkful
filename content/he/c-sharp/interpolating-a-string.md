---
title:                "תיבטדיל את המחרוזת"
html_title:           "C#: תיבטדיל את המחרוזת"
simple_title:         "תיבטדיל את המחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

מה ולמה?
אינטרפולציית מחרוזות היא תהליך של הכנסת משתנים או ערכים בתוך מחרוזת קיימת. זה משמש לנו כדי ליצור מחרוזות דינמיות ולהפעיל קוד בתוך המחרוזת. הוא מאפשר לנו לאתגר את מחרוזות קבועות ולהפעיל קוד נוסף לאורך הדרך.

כיצד לבצע:
```C#
string name = "ישראל";
int age = 73;
Console.WriteLine($"שלום, שמי הוא {name} ואני בן {age}.");
```

הדרכנות המשותפת של חצי מיליון צעדים ניתוח אסתטי
רעיונות נוספים כלולים:
1. אינטרפולציית מחרוזות ניתן להשתמש בה גם במחרוזת פעולות לעריכת מחרוזות נוספות.
2. אלמנטים נוספים יכולים להשתמש בחלק של אינטרפולציה בתוך מחרוזת אחרת.
3. למדנו איך להשתמש בקלט ליצירת מחרוזות המתאימות למשתנים שמזינים אותנו.

ראו גם:
https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated
https://www.tutorialsteacher.com/csharp/csharp-string-interpolation