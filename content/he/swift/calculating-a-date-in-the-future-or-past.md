---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Swift: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פעולה של גיבוב התאריך הנוכחי עם דינמיקה מסוימת (ימים, שבועות, שנים). תכנתים מבצעים זאת כדי לנהל לוגיקה בזמן (למשל, מתי לשלוח תזכורת או לחסם משתמש).

## איך לעשות:
ניתן להשתמש ב- `DateComponents` ו- `Calendar` ב- Swift כדי לחשב תאריכים בעתיד או בעבר. קוד מדגם:

```Swift
let now = Date()
let daysToAdd = 7
let futureDate = Calendar.current.date(byAdding: .day, value: daysToAdd, to: now) 
```

ובשביל לגבות את תאריך מהעבר:

```Swift
let daysToSubtract = 7
let pastDate = Calendar.current.date(byAdding: .day, value: -daysToSubtract, to: now) 
```

הפלט מהדוגמאות האלה יהיה תאריך חדש בעתיד או בעבר, התלוי במשתנה ימים להוספה או להפחתה.

## צלילה עמוקה:
1. הקשר ההיסטורי: בהם לשונות התכנות המוקדמות, חישוב תאריכים היה משימה מורכבת ודראית. Swift, עם את הסביבה המובנית לניהול זמן, עשתה את זה הרבה יותר פשוט.
2. חלופות: חלוף אחד ל `Calendar.current.date(byAdding:value:to:)` הוא להשתמש ב- `DateComponents` למעשה את החידושים. אפשר להשתמש ב- `DateComponents` לשינוי ימים, חודשים, שנים ועוד.
3. פרטי המימוש: `Calendar.current.date(byAdding:value:to:)` מוסיף את הערך המצוין ליחידה המצוינת, לתאריך המצוין, ומחזיר את התאריך החדש.

## ראה גם:
1. ["מדריך למתכנת Swift"](https://docs.swift.org/swift-book/) – המדריך הרשמי של Apple לשפת Swift.