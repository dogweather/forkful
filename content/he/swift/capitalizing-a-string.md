---
title:                "הפיכת מחרוזת לאותיות גדולות"
html_title:           "Swift: הפיכת מחרוזת לאותיות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות גדולות"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
האיפיון של מחרוזת הוא פעולה שבה כל אות במחרוזת הופך לאות גדולה. מתכנתים משתמשים בזה כדי להפוך את התוכן ליותר קריא או למרכז את הטקסט.

## איך לעשות?
הנה דוגמה של קוד בשפת Swift שמאפיין מחרוזת:
```Swift
let smallString = "בדיקה"
let capitalizedString = smallString.uppercased()
print(capitalizedString)
// Outputs: "בדיקה"
```
עבודה זו משנה את כל האותיות הקטנות במחרוזת לאותיות גדולות.

## הפלונגה
האיפיון של מחרוזות התחיל בכדי למרכז את הטקסט במסמכים. ישנם שיטות חלופיות, כולל כיתוב עם אותיות גדולות ידנית או שימוש בספריות חיצוניות. הגרסה הנוכחית של Swift מכילה פעולה מובנית שמאפשרת לך להפוך את כל האותיות במחרוזת לאותיות גדולות.

## ראה גם
- [דוקומנטציה של Swift](https://developer.apple.com/documentation/swift)
- [StackOverflow: איך להפוך מחרוזת לאותיות גדולות ב-Swift](https://stackoverflow.com/questions/26350709/upper-case-string-in-swift)