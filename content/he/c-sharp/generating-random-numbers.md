---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
מספרים אקראיים זה רעיון של מספר בלתי צפוי או תלוי. מתכנתים מייצרים מספרים אקראיים על מנת לחשוף נתונים באופן אקראי, להפקת מספרים באופא הגרלתי או במקרים שונים אחרים שאי אפשר להבא ציפייה מהם.

## איך עושים זאת:
בC# אתה משתמש בספרייה בשם `System` ובמחלקהבשם` Random`. שורת קוד לדוגמה:

```C#
Random rnd = new Random();
int number = rnd.Next();
Console.WriteLine(number);
```
החלפת השורות תוציא מספר אקראי.

## צלילה מעמיקה
בהיסטוריה, המחשבים מייצרים מספרים באוזן אקראית, אך זה שונה עכשיו. היו שיטות אחרות לייצור של מספרים אקראיים, אך האבטחה שלהן הייתה מופחתה. הרעיון העכשווי עובד באופן שימושי יותר - המחלקה `Random` מגרילה מספר אקראי מתוך טווח שנקבע.

## ראה גם
- [התיעוד הרשמי של `Random`](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [הדרכה לגבי מספרים אקראיים](https://www.csharp-examples.net/random-number/)