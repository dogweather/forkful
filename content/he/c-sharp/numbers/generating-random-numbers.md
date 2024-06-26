---
date: 2024-01-27 20:34:06.964462-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05D3\u05E8\
  \u05DA \u05D4\u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\u05D9\u05D5\u05EA\u05E8 \u05DC\
  \u05E4\u05D9\u05EA\u05D5\u05D7 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\u05E7\
  \u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-C# \u05D4\u05D9\u05D0 \u05D1\u05D0\u05DE\u05E6\
  \u05E2\u05D5\u05EA \u05D4\u05DB\u05D9\u05EA\u05D4 `System.Random`. \u05D4\u05E0\u05D4\
  \ \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D4\u05DE\u05DE\
  \u05D7\u05D9\u05E9\u05D4 \u05D0\u05EA \u05E9\u05D9\u05DE\u05D5\u05E9\u05D4."
lastmod: '2024-03-13T22:44:39.335884-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05D3\u05E8\u05DA \u05D4\u05E0\u05E4\u05D5\u05E6\u05D4 \u05D1\u05D9\
  \u05D5\u05EA\u05E8 \u05DC\u05E4\u05D9\u05EA\u05D5\u05D7 \u05DE\u05E1\u05E4\u05E8\
  \u05D9\u05DD \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-C# \u05D4\u05D9\u05D0\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DB\u05D9\u05EA\u05D4 `System.Random`."
title: "\u05D2\u05D9\u05DC\u05D5\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
weight: 12
---

## איך לעשות:
הדרך הנפוצה ביותר לפיתוח מספרים אקראיים ב-C# היא באמצעות הכיתה `System.Random`. הנה דוגמה פשוטה הממחישה את שימושה:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // מייצר מספר בין 1 ל-99
        Console.WriteLine($"מספר אקראי: {randomNumber}");
    }
}
```

התוצאה תהיה מספר אקראי כמו:

```
מספר אקראי: 42
```

לייצור מספר נקודה צפה אקראי בין 0.0 ל-1.0, ניתן להשתמש במתודה `NextDouble`:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"מספר נקודה צפה אקראי: {randomDouble}");
```

אם אתם עובדים על יישום רגיש לביטחון שדורש אקראיות קריפטוגרפית, עדיף להשתמש בכיתה `RNGCryptoServiceProvider` הנמצאת ב-`System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // יוצר מספר אקראי באורך 4-בתים
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"מספר אקראי בטוח קריפטוגרפית: {value}");
    }
}
```

## ניתוח מעמיק
הפקת מספרים אקראיים ב-C# התפתחה עם השנים. בתחילה, הכיתה `System.Random` הייתה הנבחרת ליצירת מספרים פסבדו-אקראיים. זהו פסבדו-אקראי מכיוון שבהינתן ערך זרע מסוים, היא תייצר את אותה הסדרה של מספרים, דבר שיכול להיות שימושי לניפוי באגים או לחזרתיות של בדיקות.

על אף שמספיק לצרכים בסיסיים, `System.Random` אינה בטוחה לשימוש מרובה תהליכים ויכולה לייצר תוצאות צפויות, שאינן מתאימות ליישומים התלויים באבטחה. מגבלה זו הובילה להצגת הכיתה `RNGCryptoServiceProvider` לאקראיות קריפטוגרפית, שהיא יותר בטוחה אך גם דורשת יותר משאבים.

חלופה ב-.NET Core ו-.NET 5+ היא הכיתה `RandomNumberGenerator` ב-`System.Security.Cryptography` לייצור מספרים אקראיים בצורה בטוחה, שמתכוונת להיות אופציה יותר מודרנית ונוחה לשימוש בהשוואה ל-`RNGCryptoServiceProvider`.

לכל שיטה של ייצור מספרים אקראיים ב-C# יש את מקומה בהתאם לדרישות היישום. לרוב היישומים, `System.Random` מספיקה, אך לאלו שדורשים מספרים אקראיים בטוחים ובלתי צפויים, הכיתות הקריפטוגרפיות מספקות חלופה עמידה.
