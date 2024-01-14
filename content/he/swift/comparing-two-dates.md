---
title:    "Swift: השוואת שתי תאריכים"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

##למה
השוואת שתי תאריכים היא כלי חשוב בתכנות של שפת Swift כי יכולה להיות מאוד שימושית לבדיקה וטיפול בתאריכים שונים במערכת הפעלה.

## כיצד לעשות זאת
עבור השוואת שתי תאריכים ניתן להשתמש בפעולת השוואה "==", ">=", "<=" וכו'. לדוגמא:
```Swift
let date1 = Date() //תאריך היום
let date2 = Date(timeIntervalSinceReferenceDate: 634119038) //תאריך כלשהו לדוגמא: 12 בנובמבר 2020

if date1 >= date2 {
    print("התאריך הראשון מאוחר יותר מהתאריך השני.")
} else {
    print("התאריך הראשון מוקדם יותר מהתאריך השני.")
}

// תוצאה: התאריך הראשון מאוחר יותר מהתאריך השני.
```

## עומק
כאשר משווים שני תאריכים, ניתן להשתמש גם בפונקציות נוספות כמו "compare" ו-"timeIntervalSince". כמו כן, קיימים גם מודפים לתאריכים כגון פורמט ואיזור זמן שימושיים לטיפול בתאריכים בצורה מדויקת יותר.

##ראה גם
- [תיעוד רשמי של שפת Swift על מנת לדעת כיצד להשתמש בפונקציות קיצון](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID331)
- [מדריך על שימוש בפונקציות זמן בשפת Swift](https://medium.com/better-programming/time-functions-in-swift-7e94daf5b1f8)
- [מפרט רשמי של מודפים לתאריכים בשפת Swift](https://github.com/apple/swift-evolution/blob/master/proposals/0088-libdispatch-for-swift3.md#new-apis-for-date-and-time-handling)