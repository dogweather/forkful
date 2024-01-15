---
title:                "לקבלת תאריך נוכחי - תאריך הנוכחי נמצא"
html_title:           "Swift: לקבלת תאריך נוכחי - תאריך הנוכחי נמצא"
simple_title:         "לקבלת תאריך נוכחי - תאריך הנוכחי נמצא"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה
מה קורה כאשר אתה מחפש את התאריך הנוכחי באייפון שלך? ייתכן שאתה צריך לתכנת יישומים או פשוט רוצה לדעת מה היום הוא. במאמר זה, אנו נדבר על כיצד לקבל את התאריך הנוכחי באמצעות שפת Swift הנוכחית, עם דוגמאות קוד והסברים עמוקים.

## כיצד לעשות זאת
תחילה, נצטרך לייבא את הספרייה "Foundation" באמצעות הפקודה הבאה:
```Swift
import Foundation
```

אם אנו רוצים לקבל את התאריך הנוכחי בפורמט מסוים, כמו "dd/MM/yyyy", אנו יכולים להשתמש במחלקת "DateFormatter" כדי להמיר את התאריך למחרוזת מתאימה. הנה דוגמה של כיצד לקבל את התאריך הנוכחי בפורמט שדיברנו עליו:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let today = Date()
let currentDate = dateFormatter.string(from: today)

print(currentDate)

// Output: 02/09/2019
```

אם ברצונך לקבל את התאריך עם שעה ותאריך מלאים, אפשר להשתמש בפורמט הבא: "dd/MM/yyyy HH:mm:ss". נהניתי להשתמש בפונקציה לשדרוג, שמוסיפה את האפשרות להדפיס את השנה כמו שעה או דקות. הנה דוגמה לשימוש בפורמט זה:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy HH:mm:ss"
let today = Date()
let currentDate = dateFormatter.string(from: today)

print(currentDate)

// Output: 02/09/2019 15:27:03
```

על מנת לקבל את התאריך הנוכחי עם אזור זמן מסוים, אנו ניקח עזרה ממחלקת "TimeZone". הנה דוגמה לאיך לקבל את התאריך הנוכחי באזור זמן של ישראל:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy HH:mm:ss"
dateFormatter.timeZone = TimeZone(identifier: "Asia/Jerusalem")
let today = Date()
let currentDate = dateFormatter.string(from: today)

print(currentDate)

// Output: 02/09/2019 23:27:03
```

## חקור עמוק יותר
כאשר אנו משתמשים במחלקת "DateFormatter", אנו ניצור מופע חדש של המחל