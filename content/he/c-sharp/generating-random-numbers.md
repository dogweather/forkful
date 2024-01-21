---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:12.418235-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים היא כלי שמשתמשים בו לחיקוי אירועים לא צפויים או לבחירות באופן חסר דעת קודמת. תכניתנים משתמשים בזה למבחנים, משחקים, בטיחות, ועוד.

## איך לעשות:
```C#
using System;

class RandomExample {
    static void Main() {
        Random rnd = new Random();
        int randomNum = rnd.Next(); // אם רוצים מספר בטווח מסוים, למשל 0-99, שימוש ב: rnd.Next(100)
        Console.WriteLine(randomNum);
    }
}
```
פלט לדוגמה:
```
163945688
```
## עיון רחב יותר:
בעבר, היינו משתמשים בתיאוריות של כאוס ואלגוריתמים מרובים ליצירת מספרים חסרי סדר. כיום, יש לנו אלגוריתמים מתקדמים יותר כמו Mersenne Twister. חשוב לזכור שמחלקת `Random` ב-C# היא פסבדו-אקראית, ולא אמיתית אקראית – לשימושים שדורשים ביטחון גבוה יותר יש להשתמש ב-RNGCryptoServiceProvider. כמו כן, יש להיזהר מיצירת עצם `Random` חדש בכל פעם, שכן זה יכול לגרום לחזרתיות במספרים בריצות מהירות.

## גם כדאי לראות:
- [Microsoft Random Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0) – מסמך רשמי על מחלקת Random ב-C#.
- [RNGCryptoServiceProvider Class](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-6.0) – על אופן השימוש ב-RNGCryptoServiceProvider ליצירת מספרים אקראיים בטוחים יותר.
- [Random Number Generation on Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation) – הסבר על יצירת מספרים אקראיים ואלגוריתמים נפוצים.