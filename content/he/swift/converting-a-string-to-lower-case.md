---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
ביקשת להמיר מחרוזת לאותיות קטנות? זו שיטה פרקטית בידי מתכנתים בשביל להגדיל את עמידות המערכת במול נקניקיות משתמשים—כמו קלט עם או בלי אותיות גדולות.

## איך לעשות את זה:
הפונקציה `lowercased()` מתאימה למשימה! תיקח מחרוזת כקלט ותחזיר גרסה שלה באותיות קטנות. הנה דוגמא:

```Swift
let originalString = "Swift Programming"
let lowercasedString = originalString.lowercased()
print(lowercasedString)
```

פלט שהוא למעשה:

```Swift
"swift programming"
```

## עומק יותר:
ממירת מחרוזות לאותיות קטנות הייתה פרקטיקה בתכנות מאז תקופת מחשבי המיינפריים. בדרך זו, מאגרי מידע ומערכות נתונים הפכו לעמידים יותר בפני שגיאות כתיב של משתמשים.

אלטרנטיבה מקובלת לממירה לאותיות קטנות היא "נרמול". מתודה זו מאפשרת לנו להמיר הכול לאותיות גדולות, או להביא לאיזון בין השניים.

## ראה גם:
- [התיעוד הרשמי של Swift על `lowercased`](https://developer.apple.com/documentation/swift/string/2293861-lowercased)
- [מאמר מעניין שמתאר טכניקות שונות של מילול](https://en.wikipedia.org/wiki/Letter_case#Case_folding_and_case_conversion)