---
title:                "Haskell: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה

הקוד הפותחים והמתכנתים שלנו צריכים להשתמש בחישוב תאריך בעתיד או בעבר בכמה מקרים בתוכניותינו. לדוגמה, אם אנחנו מפתחים אפליקציה של תזכורות ורוצים להדגיש למשתמש על אירוע הקרוב המתקרב, או אם אנחנו רוצים ליצור פונקציה שתחזיר לנו את תאריך הלידה של בני המשפחה שלנו. כתוצאה מכך, חישוב תאריך בעתיד או בעבר יכול לסייע לנו ליצור יישום יעיל ומספק למשתמשים שלנו.

## איך לעשות זאת

ב-Haskell יש כמה דרכים לחשב תאריך בעתיד או בעבר. למשל, ניתן להשתמש בפונקציות כמו `addDays` ו-`addMonths` כדי להוסיף ימים או חודשים לתאריך קיים. למשל:

```Haskell
import Data.Time

-- חישוב תאריך בעתיד:
let today = fromGregorian 2021 08 23 -- תאריך היום
let futureDate = addDays 10 today -- תאריך של 10 ימים בעתיד

-- חישוב תאריך בעבר:
let pastDate = addMonths (-3) today -- תאריך של 3 חודשים בעבר
```

ניתן גם להשתמש בפונקציות חישוב של טבלאות כמו `lookup` ו-`elemIndices` לחישוב תאריך במקרים מיוחדים. למשל:

```Haskell
import Data.Time

-- חישוב תאריך לפי יום בשבוע:
let today = fromGregorian 2021 08 23
let days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
let dayOfWeek = lookup today days -- יום השבוע של התאריך היום

-- חישוב תאריך לפי תאריכי יום הולדת קודמים:
let birthdays = [fromGregorian 2001 08 23, fromGregorian 1993 08 10, fromGregorian 1990 04 05]
let previousBirthdays = elemIndices today birthdays -- תאריכי יום ההולדת הקודמים לתאריך היום
```

## העומק שבחישוב תאריך בעתיד או בעבר

במאמר זה ראינו כמה דרכים שונות לחשב תאר