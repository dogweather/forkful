---
title:                "Swift: קבלת התאריך הנוכחי"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

מדוע כדאי לך לגשת אל תאריך נוכחי בכתיבת קוד Swift?

## כיצד לעשות זאת

הנה שתי דרכים פשוטות לגשת אל תאריך נוכחי בשפת סוויפט:

```Swift
// דרך 1: שימוש במחלקת Date ומתודת its: 
let currentDate = Date()
print(currentDate)

// דרך 2: שימוש בפונקציה Date():
let currentDate = Date()
print(currentDate)
```

כאן, אנו משתמשים בשתי דרכים ליצירת משתנה המכיל תאריך נוכחי, ואנו מדפיסים אותו למסך כדי לוודא שהתוצאות זהות.

## חפירה עמוקה

בכל יום נשתמע דרושות לגשת אל תאריך נוכחי בשיטה שונה. למקרים אלו, השיטה Date() יכולה לספק תוצאות בלתי צפויות. כדי להשתמש בתאריך נוכחי בצורה מדויקת יותר, ניתן להשתמש במחלקת Calendar ולהחשב על הפתרונות האפשריים לכל מקום נמצא. בדרושה מעשי יותר, ניתן לשתמש בפונקציות שימושיות נוספות כגון DateFormatter כדי לפורמט את התאריך לפי הרצונות שלך.

## ראה גם

כאן מצורפים כמה קישורים נוספים העוסקים בנושא גישת תאריך נוכחי:

- [מסמך רשמי של Apple על המחלקה Date](https://developer.apple.com/documentation/foundation/date)
- [בלוג מאמר בעברית על שימוש בתאריך נוכחי בשפת סוויפט](https://swiftisrael.co.il/2019/11/28/date-and-time-in-swift/)
- [הסבר מעמיק יותר על השימוש במחלקת Calendar בסוויפט](https://useyourloaf.com/blog/adding-time-to-a-date-in-swift/)