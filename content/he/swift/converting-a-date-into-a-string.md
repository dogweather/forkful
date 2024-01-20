---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
תהליך המרת תאריך למחרוזת בסוויפט הוא מזיקל להמחשה של דייט בצורה הנוחה ביותר למנהלן, כמו כן הוא מבצע לאפשר חיבור מתוך יישומים שונים, שהם לרוב משתמשים במחרוזות.

## איך לעשות את זה:
הנה כמה דוגמאות לקוד בסוויפט המראות כיצד להמיר תאריך למחרוזת:
```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()

formatter.dateFormat = "yyyy/MM/dd HH:mm"
let result1 = formatter.string(from: date)
print(result1)  // "2022/10/03 13:15"

formatter.dateFormat = "EE dd, MMM yyyy"
let result2 = formatter.string(from: date)
print(result2)  // "Mon 03, Oct 2022"
```
בכוד הזה, אנו מאתחלים את `DateFormatter`, נקבע את מסגרת התאריך ולבסוף אנו משתמשים בפונקציה `string(from:)` לשחזור בתאריך כמחרוזת.

## צולול עמוקה
המרת תאריכים למחרוזות באים במגוון נרחב של מסגרות זמנים שמשמשים את הצרכים של המתכנת שונות. סוויפט נושא היסטוריה של דרך מרה תאריכים למחרוזות, כולל את המשתמש `NSDateFormatter` עד Swift 2, והמרת סטנדרטית תאריכים למחרוזות מאז Swift 3.

אלטרנטיבות משתנה על פי הרוב במסגרת של הצעד הבא בקוד שלך. לדוגמה, במקרה אם ברצונך להשתמש בתאריך באובייקט JSON, יתכן שתרצה להשתמש במרת `ISO8601DateFormatter`.

מימוש הפרטים מאחורי הקלעים של `DateFormatter` הם מתוך יכולת Apple ו-Swift לעסוק במקרים מגוונים של תאריכים, מאפשרת לך להתמיד את זמן המחשב שלך וזמן המחשב של מי שמקבל את המחרוזת.

## ראה גם:
- [מדריך רשמי מ-Apple ל-DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [יעילות של DateFormatter](https://www.swiftbysundell.com/articles/working-with-dates-in-swift/)
- [רשימה מלאה של Date Formats](http://waracle.com/iphone-nsdateformatter-date-formatting-table/)