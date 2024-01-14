---
title:    "Swift: חישוב תאריך בעתיד או בעבר"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##מדוע

בשפת סוויפט ניתן לחשב תאריך בעתיד או בעבר בפשטות בעזרת כמה שורות קוד. ייתכן שתרצו להשתמש בתכנות זה במסגרת שימוש עם תאריך מסוים או לצורך בניית אפליקציות ששומרות תאריכים.

## איך לעשות זאת

משתמשים בחבילת התאריכים (Dates) ניתן לבצע חישובים של תאריך בעתיד או בעבר באמצעות הפונקציות המתאימות.

```Swift 
let monthsToAdd = 5

let currentDate = Date()

let futureDate = Calendar.current.date(byAdding: .month, value: monthsToAdd, to: currentDate)

// Output: futureDate = Optional(2020-08-10 06:52:13 +0000)
```

כפי שאתם יכולים לראות, ביצירת משתנה התאריך העתידי ניתן להשתמש בפונקציית הקוד שתאפשר לכם להוסיף או להפחית חודשים לתאריך הנוכחי.

## חפירה עמוקה

אם ברצונכם להתעמק עוד יותר בנושא החישובים של תאריכים בשפת סוויפט, ישנם עוד פונקציות ואפשרויות לחישוב תאריכים. למשל, ניתן להשתמש בפונקציית "dateComponents" אשר מאפשרת למשתמש ליצור תאריך ממספרים מדוייקים כמו יום, חודש ושנה.

```Swift 
let date = Date()

let calendar = Calendar.current

let components = calendar.dateComponents([.year, .month, .day], from: date)

// Output: components = Optional(year: 2020, month: 3, day: 10, era: nil, quarter: nil, weekOfMonth: nil, weekOfYear: nil, yearForWeekOfYear: nil, weekday: nil, weekdayOrdinal: nil, year: nil, timeZone: nil)
```

בנוסף, ניתן להשתמש בפונקציית "dateFormatter" על מנת לבצע עיצוב והצגת התאריך בצורה הרצויה.

##ראו גם

כאשר אתם מתכנתים בשפת סוויפט, חישוב תאריך בעתיד או בעבר יכול להיות מאוד מועיל. ככל שתעמיקו ותתרגלו בשימוש בפונקציות התאר