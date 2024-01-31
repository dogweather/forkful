---
title:                "שימוש במערכים אסוציאטיביים"
date:                  2024-01-30T19:10:42.515100-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במערכים אסוציאטיביים"
programming_language: "C#"
category:             "C#"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
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
