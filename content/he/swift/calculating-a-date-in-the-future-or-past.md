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

## למה

אנשים עשויים להשתמש בחישוב תאריכים בעתיד או בעבר כדי לתכנן אירועים ולדעת מתי דברים ספציפיים יתרחשו.

## איך לעשות

לחישוב תאריך בעתיד או בעבר באמצעות שפת סוויפט ניתן להשתמש בפונקציה `Calendar.date(byAdding:to:)`. לדוגמה, נרצה לחשב מתי תיהיה התאריך האוטומטי הבא שנתן יום ראשון עבור תאריך ספציפי. הנה קוד בסוויפט שיעשה זאת עבורנו:

```Swift
let calendar = Calendar.current
let date = Date()  // תאריך נוכחי
let nextSunday = calendar.date(byAdding: .weekday, value: 1, to: date)  // חישוב תאריך שבת הבאה
```

בתוך המשתנה `nextSunday` נקבל את התאריך האוטומטי הבא שיומו הראשון שלו יהיה תאריך ספציפי. ניתן להחתים ערכים לשדות הנדרשים כפי הצורך, לדוגמה אם נרצה לחשב מתי יהיה תאריך שבת הבאה לאחר חודש או שבועות מתאריך מסוים.

## חקירה מעמיקה

השפה שוויפט מספקת שיטה פשוטה ויעילה לחישוב תאריכים בעתיד או בעבר. בשימוש בפונקציות המתאימות ובכלים הנכונים, ניתן להתאים את חישובי התאריך לכל מטרה ספציפית. כמו כן, חשוב להיזהר ולוודא כי התאריך המחושב נכון כדי למנוע שגיאות וטעויות.

## ראה גם

- [תיעוד Swift רשמי עבור פונקציות בנושא תאריכים](https://developer.apple.com/documentation/foundation/calendar)
- [מדריך בנושא חישוב תאריך בסוויפט](https://www.hackingwithswift.com/read/15/1/creating-flexible-strings-from-dates-with-datetimeformatter)