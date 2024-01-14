---
title:    "Swift: חישוב תאריך בעתיד או בעבר"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## למה

כתיבת קאסטום טמפלייט לתאריך בעתיד או בעבר יכולה להיות כלי מועיל בתוכנות עסקיות או אפליקציות עבור משתמשים שמעוניינים לקבל מידע עתידי או עברי על אירועים או משימות מסוימים.

## איך לעשות זאת

ראשית, נצטרך להגדיר משתנה מסוג `Date` המכיל את התאריך הנוכחי. לדוגמה:

```Swift
let currentDate = Date()
```

לאחר מכן, נשתמש בפונקציה `Calendar.current.date(byAdding:value:to:)` כדי להוסיף ערך מסוים לתאריך הנוכחי וליצור תאריך חדש. לדוגמה, אם נרצה להחזיר את התאריך של מחר ניתן להשתמש בפונקציה כדי להוסיף יום אחד לתאריך הנוכחי:

```Swift
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: currentDate)
```

לבסוף, כדי להציג את התאריך בפורמט מסוים נשתמש ב- `DateFormatter` ונגדיר את הפורמט הרצוי. לדוגמה, אם נרצה להציג את התאריך בפורמט של חודש, יום ושנה ניתן לעשות זאת באמצעות הקוד הבא:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "MMMM dd, yyyy"
let formattedDate = formatter.string(from: tomorrow)
```

התוצאה תהיה: "יולי 21, 2021"

## למעמק

קביעת תאריך בעתיד או בעבר מורכב יותר ממה שנראה. על מנת לקבוע את התאריך בפורמט מיוחד עלינו להשתמש ב- `Calendar` ו- `DateComponents`. נצטרך גם להתייחס למיקום גיאוגרפי כדי לתאם את התאריך לתאריך מקומי.

לדוגמה, אם נרצה לקבוע את התאריך ליום ראשון הבא שבו יהיה 15 בחודש ינואר של השנה הבאה עלינו לכתוב את הקוד הבא:

```Swift
let calendar = Calendar(identifier: .gregorian)
var components = DateComponents()
components.year = 2022
components.month = 1
components.weekday = 1
components.weekdayOrdinal = 3

if let futureDate = calendar