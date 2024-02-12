---
title:                "גילוי מספרים אקראיים"
aliases:
- /he/c-sharp/generating-random-numbers/
date:                  2024-01-27T20:34:06.964462-07:00
model:                 gpt-4-0125-preview
simple_title:         "גילוי מספרים אקראיים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

פיתוח מספרים אקראיים ב-C# כולל יצירת ערכים מספריים בלתי צפויים בתחום נתון. מתכנתים משתמשים בשיטות אלו ליישום תכונות כמו קריפטוגרפיה, סימולציות ומשחקים, שבהם נדרשת בלתי צפיה או הדמיה של אקראיות מהעולם האמיתי.

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
