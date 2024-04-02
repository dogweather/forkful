---
date: 2024-01-26 03:43:55.155738-07:00
description: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05DE\u05E2\u05D5 \u05D4\u05EA\u05D0\u05DE\u05EA\u05DD \u05DC\u05E2\
  \u05E8\u05DA \u05D4\u05DE\u05E7\u05D5\u05DD \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\
  \u05D9\u05D5\u05EA\u05E8 - \u05D7\u05E9\u05D1\u05D5 \u05E2\u05DC \u05D6\u05D4 \u05DB\
  \u05E2\u05DC \u05DC\u05DB\u05D5\u05D5\u05E5 \u05D0\u05D5\u05EA\u05DD \u05DC\u05E6\
  \u05D5\u05E8\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD \u05DB\
  \u05D3\u05D9 \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D3\u05D9\u05D5\u05E7, \u05DC\
  \u05E9\u05E4\u05E8 \u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05DD, \u05D0\u05D5 \u05DB\
  \u05D0\u05E9\u05E8\u2026"
lastmod: '2024-03-13T22:44:39.334233-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\
  \u05E9\u05DE\u05E2\u05D5 \u05D4\u05EA\u05D0\u05DE\u05EA\u05DD \u05DC\u05E2\u05E8\
  \u05DA \u05D4\u05DE\u05E7\u05D5\u05DD \u05D4\u05E7\u05E8\u05D5\u05D1 \u05D1\u05D9\
  \u05D5\u05EA\u05E8 - \u05D7\u05E9\u05D1\u05D5 \u05E2\u05DC \u05D6\u05D4 \u05DB\u05E2\
  \u05DC \u05DC\u05DB\u05D5\u05D5\u05E5 \u05D0\u05D5\u05EA\u05DD \u05DC\u05E6\u05D5\
  \u05E8\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E2\u05D2\u05DC\u05D9\u05DD \u05DB\u05D3\
  \u05D9 \u05DC\u05E9\u05DC\u05D5\u05D8 \u05D1\u05D3\u05D9\u05D5\u05E7, \u05DC\u05E9\
  \u05E4\u05E8 \u05D1\u05D9\u05E6\u05D5\u05E2\u05D9\u05DD, \u05D0\u05D5 \u05DB\u05D0\
  \u05E9\u05E8\u2026"
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## מה ולמה?
עיגול מספרים משמעו התאמתם לערך המקום הקרוב ביותר - חשבו על זה כעל לכווץ אותם לצורה פשוטה יותר. מתכנתים מעגלים כדי לשלוט בדיוק, לשפר ביצועים, או כאשר מציגים תוצאות ידידותיות למשתמש - כמו מחירים שאין צורך בשלושה מקומות עשרוניים.

## איך לעשות:
הנה הכרטיס המושלם לעיגול מספרים ב-C#:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // עיגול למספר שלם הקרוב ביותר
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // פלט: 123

        // ציון מספר המקומות העשרוניים
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // פלט: 123.46

        // עיגול למעלה בלי תלות בספרה הבאה
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // פלט: 124

        // עיגול למטה בלי תלות בספרה הבאה
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // פלט: 123
    }
}
```

## צלילה עמוקה
בימים ההם, עיגול היה משימה פשוטה לקיצוץ עלויות חישוב. כל מחזור ספירה נספר, וקיצוץ מספרים חסך זמן יקר. קפיצה ל-C# המודרני, והעניין הוא לנהל כפולים ועשרוניים בעלי נטייה נודעת לשגיאות דיוק וחוסרים בהצגה.

מעבר ל-`Math.Round`, `Math.Floor` ו-`Math.Ceiling`, ה-enumeration `MidpointRounding` מאפשר לנו לקבוע את גורל הספרות המרכזיות העניות - זהו צומת הדרכים בין כללי הבנקאות להוגנות של "עגל חצי למעלה".

לקהלים קשים יותר, כמו יישומי מתמטיקה או פיננסים רציניים, יש לנו את `decimal` על פני `double`, מקטין דרמה של עיגול על ידי הצעת דיוק גבוה יותר - פחות עיגולים, פחות בעיות.

## ראו גם
- [מסמכי C# הרשמיים על `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: מתי כדאי לי להשתמש ב-Double במקום ב-Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [תקן IEEE לחישוב נקודה צפה (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
