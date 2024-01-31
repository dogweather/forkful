---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
גדלת אותיות במחרוזת פירושו להפוך את כל האותיות לאותיות גדולות. תכניתנים עושים זאת לצורך עקביות, הדגשה או כחלק מפורמט שדה קלט.

## How to: (איך לעשות:)
ב-Elm אין פונקציה פנימית לגדלת מחרוזת, אבל זה פשוט ליישם בעצמך. נשתמש ב-package שנוסף לשפה:

```Elm
import String

capitalize : String -> String
capitalize str =
    String.toUpper str

-- דוגמת שימוש
main =
    capitalize "hebrew"
    -- פלט: "HEBREW"
```

## Deep Dive (עומק הנושא)
Elm לא מציע המרת אותיות קטנות לגדולות באופן ישיר ב-`core libraries`, אבל תוכל להשתמש ב-`String.toUpper` מ-package קהילתי כמו `elm-string-transform`. לשוניות אחרות כמו JavaScript כן כוללות פונקציונליות זו ישירות, כאשר `toUpperCase` היא שיטה פנימית של אובייקט ה-string.

התיקון מצריך שימוש ב-package חיצוני או כתיבת פונקציה באופן ידני. יתרה מזו, כאשר אתה משתמש בהמרה לאותיות גדולות, זכור שזה יכול להשפיע על מיון והשוואות בינלאומיות, שכן אותיות מהגדלות בשפות שונות יכולות להיות סוג של תווים שונים לחלוטין.

## See Also (ראה גם)
- Elm `String` documentation: [String](https://package.elm-lang.org/packages/elm/core/latest/String)
