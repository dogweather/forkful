---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?: 

מציאת אורך של מחרוזת היא תהליך שבו אנו מספרים את מספר התווים במחרודת. מתכנתים זקוקים לכך במגוון עתים, כולל בדיקת תקינות, ביצוע השוואות או הכנת קלט למערכת.

## כיצד לבצע:

ב- C#, אנו משתמשים במאפיין `Length` של מחרוזת כדי לקבל את אורך המחרוזת:

```C#
string myString = "שלום, עולם!";
int length = myString.Length; 
Console.WriteLine(length); // הפלט: 11
```

כאן, `myString.Length` מחזיר את אורך המחרוזת "שלום, עולם!", שהוא 11.

## צלילה עמוקה: 

המאפיין `Length` ב-C# הוא חלק ממחלקת `String` שמוגדרת ב-.NET Framework מהיום הראשון שלו. ישנם דרכים חלופיות למציאת אורך מחרוזת, כמו למשל על ידי שימוש בלולאה `for` כדי לרוץ על כל התווים, אבל הן אינן יעילות כמו שימוש ב-`Length`.

בניגוד לשפות אחרות, ב-C# אורכו של משפט נמדד במספר התווים ולא במספר הבתים שהוא תופס בזיכרון. אז, במקרה של מחרוזת Unicode, אורך המחרוזת יציין את מספר התווים Unicode, לא את מספר הבתים.

## ראה גם:

1. [פונקציות נוספות של מחלקת String ב-C#.NET](https://docs.microsoft.com/he-il/dotnet/api/system.string?view=net-5.0)
2. [חלופות לקבלת אורך מחרוזת ב-C#](https://stackoverflow.com/questions/705355/how-can-one-get-string-length-in-c)
3. [C# Programming Guide](https://docs.microsoft.com/en-us/dotnet/csharp/)