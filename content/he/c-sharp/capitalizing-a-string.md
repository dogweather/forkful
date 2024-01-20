---
title:                "הגדלת אות הראשונה במחרוזת"
html_title:           "C#: הגדלת אות הראשונה במחרוזת"
simple_title:         "הגדלת אות הראשונה במחרוזת"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות גדולות, כמו "HELLO", משמשת מתכנתים להבטיח שהטקסט מוצג באופן אחיד בכל מקום, בלי קשר לצורתו המקורית. זה מקל ומספק ברורים תכניתיים.

## איך:
```C#
string myString = "hello";
string upperCaseString = myString.ToUpper();
Console.WriteLine(upperCaseString); // Outputs: HELLO
```
הדוגמא מראה את הפיכת מחרוזת לאותיות גדולות באמצעות הפונקציה `ToUpper()`.

## צלילה עמוקה:
הפיכת מחרוזת לאותיות גדולות הייתה נפוצה מאוד בימי הקובול, כאשר מערכות ההפעלה התמכו רק באותיות גדולות. ישנם אלטרנטיבות ל`ToUpper()`, כמו `ToUpperInvariant()`, אשר מספקת דרך יותר הולמת להעלאת טקסט בשפות שונות. `ToUpper()` משנה את המחרוזת לאותיות גדולות לפי הגדרות הלוקאליזציה של המערכת, בעוד `ToUpperInvariant()` משתמשת בהגדרות התחשיב המשתמש.

## ראה גם:
[Microsoft Documentation – ToUpper](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)  
[Microsoft Documentation – ToUpperInvariant](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant?view=net-5.0)