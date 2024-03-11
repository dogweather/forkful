---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:42.515100-07:00
description: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DE\u05D9\u05DC\u05D5\u05E0\
  \u05D9\u05DD \u05D1-C#, \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DA\
  \ \u05DC\u05D0\u05D7\u05E1\u05DF \u05D5\u05DC\u05E0\u05D4\u05DC \u05D6\u05D5\u05D2\
  \u05D5\u05EA \u05E9\u05DC \u05DE\u05E4\u05EA\u05D7\u05D5\u05EA \u05D5\u05E2\u05E8\
  \u05DB\u05D9\u05DD. \u05D4\u05DD \u05D4\u05D0\u05E4\u05E9\u05E8\u05D5\u05EA \u05E9\
  \u05DC\u05DA \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05E6\u05E8\u05D9\u05DA\
  \ \u05DC\u05D0\u05D7\u05D6\u05E8 \u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05DE\u05D4\
  \u05D9\u05E8\u05D5\u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05DE\
  \u05D6\u05D4\u05D4\u2026"
lastmod: '2024-03-11T00:14:12.782023-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DE\u05D9\u05DC\u05D5\u05E0\
  \u05D9\u05DD \u05D1-C#, \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DA\
  \ \u05DC\u05D0\u05D7\u05E1\u05DF \u05D5\u05DC\u05E0\u05D4\u05DC \u05D6\u05D5\u05D2\
  \u05D5\u05EA \u05E9\u05DC \u05DE\u05E4\u05EA\u05D7\u05D5\u05EA \u05D5\u05E2\u05E8\
  \u05DB\u05D9\u05DD. \u05D4\u05DD \u05D4\u05D0\u05E4\u05E9\u05E8\u05D5\u05EA \u05E9\
  \u05DC\u05DA \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05E6\u05E8\u05D9\u05DA\
  \ \u05DC\u05D0\u05D7\u05D6\u05E8 \u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05DE\u05D4\
  \u05D9\u05E8\u05D5\u05EA \u05D1\u05D4\u05EA\u05D1\u05E1\u05E1 \u05E2\u05DC \u05DE\
  \u05D6\u05D4\u05D4\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, או מילונים ב-C#, מאפשרים לך לאחסן ולנהל זוגות של מפתחות וערכים. הם האפשרות שלך כאשר אתה צריך לאחזר ערכים במהירות בהתבסס על מזהה ייחודי, מה שהופך את ניהול הנתונים לקל יותר ביישומים מורכבים.

## איך לעשות:

ב-C#, אתה עובד עם מערכים אסוציאטיביים באמצעות המחלקה `Dictionary<TKey, TValue>`. הנה דוגמה מהירה להתחלה:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // יצירת מילון
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // הוספת זוגות מפתח-ערך
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // גישה לערך באמצעות המפתח שלו
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // עדכון ערך
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // הסרת זוג מפתח-ערך
        fruitBasket.Remove("Oranges");

        // סיבוב על המילון
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
פלט לדוגמה:
```
Apples: 5
Updated Apples: 7
Apples: 7
```

הדוגמה הזו מציגה יצירת מילון, הוספה, גישה, עדכון, והסרת אלמנטים, וסיבוב עליו.

## עיון נוסף

המושג של מערכים אסוציאטיביים חוזר לשימושם בשפות תסריט כמו Perl ו-PHP, שם הם מציעים גמישות בניהול אוספי נתונים. ב-C#, `Dictionary<TKey, TValue>` היא המימוש המקובל, שהוצג בגרסה 2.0 של .NET Framework. היא מאחסנת נתונים בטבלת גיבוב, מה שמבטיח חיפושים, הוספות, ומחיקות יעילות.

עם זאת, כדאי לציין שלמרות שמילונים הם מאוד גמישים, הם לא תמיד יהיו האפשרות הטובה ביותר שלך. לשמירה על אוספים ממוינים, אתה עשוי לבחון את `SortedDictionary<TKey, TValue>` או `SortedList<TKey, TValue>`, שמציעים סדר ממוין תמורת איטיות בפעולות הוספה והסרה. לסצנריות שדורשות ביטחון רב-תילי, `ConcurrentDictionary<TKey, TValue>` מוסיף עלות אך מבטיח גישה בטוחה ממספר חוטים בלי נעילה ידנית.

בסופו של דבר, הבחירה של מימוש מערך אסוציאטיבי ב-C# מותנית בצרכים הספציפיים שלך בנוגע לסדר, ביצועים, וביטחון רב-תילי.
