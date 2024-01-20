---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה? - What & Why?
פרסומת של תאריך ממחרוזת הוא מרכז פעולה בה משנים מחרוזת שמכילה תאריך ושעה למופע של `Date`. מתכנתים עושים את זה כדי לטפל במתנהג באופן קריא וחביב יותר עם נתוני תאריך ושעה.

## איך לעשות - How to:
בדוגמאות הקוד הבאות, אנו משתמשים ב- `DateFormatter` לפרסום תאריך ממחרוזת.

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let aDate = dateFormatter.date(from: "2022-09-15")

print(aDate)
```

הפלט יהיה:

```Swift
Optional(2022-09-15 00:00:00 +0000)
```

## צלילה עמוקה - Deep Dive
פונקציה של פרסום של תאריך ממחרוזת נמשך כבר הרבה שנים, והיא משפרת את מדדי המידע. אם אתה מחפש אלטרנטיבות, בוא לנסות במספר ספריות פופולריות רדום, אופטובר עם פונקציונליות זהה. בראש ובראשונה, `DateFormatter` הוא מרכז פעולה מרכזי ב- Swift, מה שאומר שהוא שולב בעומק במרכז המערכת.

## ראה גם - See Also
- [Apple's Documentation on DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)