---
title:                "Swift: חישוב תאריך בעתיד או בעבר"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# למה

מחשבון תאריכים יכול להיות כלי מועיל בתכנות ב-Swift. הוא מאפשר לנו לחשב את התאריך בעתיד או בעבר בקלות ובפשטות באמצעות קוד. ניתן להשתמש בו בשוני מצבים, כגון ייצור מועד לאירוע חשוב או חישוב התאריך של תוקף תעודה.

## איך לעשות את זה

קוד נוסף לדוגמה שיועיל בחישוב תאריך בעתיד או בעבר:

```Swift
// חישוב תאריך בעתיד - מספר ימים להוסיף
let today = Date()
let futureDate = Calendar.current.date(byAdding: .day, value: 7, to: today)
print(futureDate) // תאריך של שבוע מהיום בעתיד
```

```Swift
// חישוב תאריך בעבר - מספר ימים להפחית
let today = Date()
let pastDate = Calendar.current.date(byAdding: .day, value: -7, to: today)
print(pastDate) // תאריך של שבוע קודם מהיום בעבר
```

## חפירה עמוקה

בנוסף לחישוב תאריכים בעתיד ובעבר, ניתן להשתמש בקוד כדי לבצע פעולות מתקדמות יותר עם תאריכים. למשל, ניתן לחשב את תאריך הנוכחי על פי תבנית מסוימת, להשוות בין תאריכים שונים ולייצר טקסט בפורמט של תאריך. ישנן גם פונקציות נוספות כמו חישוב תאריך לפי שעה והמרת תאריך למחרוזת.

# ראו גם

כדי לקבל מידע נוסף על חישוב תאריכים ב-Swift, ניתן לקרוא את המסמכים הבאים:

- [מסמך רשמי על תאריכים ושעות ב-Swift](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
- [מאמר בבלוג על תאריכים ב-Swift](https://www.appcoda.com/swift-date-tutorial/)
- [דוגמאות נוספות לחישוב תאריכים ב-Swift](https://www.hackingwithswift.com/articles/86/how-to-work-with-dates-and-times-in-swift)