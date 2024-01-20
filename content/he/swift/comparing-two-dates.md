---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

##  מה זה ולמה? 

השוואה בין שני תאריכים במציאות היא תהליך בו אנו משווים שני תאריכים כדי לראות איזה מהם מגיע לפני השני, או אם שניהם שווים. תכנתים משתמשים בו כדי לאתר אירועים המתרחשים לפי סדר זמני, לנתח מדדים כמו משך זמן, ועוד.

##  שימוש: 

דוגמאות קוד ודוגמאות לפלט הקוד תחת קטעי הקוד ```Swift ... ```:

```swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"
let date1 = dateFormatter.date(from: "2023/01/01 12:00")!
let date2 = dateFormatter.date(from: "2023/01/02 13:30")!

if date1.compare(date2) == .orderedAscending {
    print("Date1 is earlier than date2.")
} else if date1.compare(date2) == .orderedSame {
    print("Both dates are same.")
} else {
    print("Date1 is later than date2.")
}
```

פלט של הדוגמא הוא:

```sh
Date1 is earlier than date2.
```

##  בחפיפה עמוקה: 

1. מאז אבי קדמוני, מתכנתים נמוכים בעיסוקים הקשורים לניהול זמן. Swift, כמו שפות אחרות, מציעה מספר אפשרויות לעזרה בטיפול בתאריכים ובזמן.
2. ישנם דרכים אחרות להשוות בין שני תאריכים ב-Swift, כולל שימוש ב- `.timeIntervalSince` כדי למצוא את מספר השניות שבין שני תאריכים.
3. הפונקציה `.compare()` מבצעת פעולה של השוואה ישירה בין שני אובייקטים של `Date` ומחזירה ערך מספרי ממנה של `ComparisonResult`.

##  ראה גם: 

1. [פוסט ב-Stackoverflow על השוואת תאריכים ב- Swift](https://stackoverflow.com/questions/26198526/how-compare-two-dates-in-swift)
2. [מדריך התכנות של Apple בנושא 'Working with Dates and Times'](https://developer.apple.com/documentation/foundation/date)
3. [מאמר מאוייר בנושא 'Understanding Dates and comparing two dates in Swift'](https://www.hackingwithswift.com/example-code/language/how-to-compare-dates-with-swift)