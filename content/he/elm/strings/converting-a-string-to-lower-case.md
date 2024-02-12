---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:38:52.826337-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרה של מחרוזת לאותיות קטנות זה פשוט לקחת טקסט ולהפוך את כל האותיות הגדולות לקטנות. תוכניתנים עושים את זה כדי לאחד פורמטים, להקל על השוואות טקסט, ולשמור על עקביות בבסיסי נתונים.

## איך לעשות:
באלם זה פשוט מאוד. תשתמשו בפונקציה `String.toLower` כדי להמיר כל מחרוזת שברצונכם.

```Elm
import String

lowercaseString : String -> String
lowercaseString str =
  String.toLower str

-- דוגמא לשימוש:
result : String
result =
  lowercaseString "HELLO, WORLD!"

-- תוצאה:
-- "hello, world!"
```

## עיון מעמיק
המרה לאותיות קטנות היא משימה סטנדרטית בתכנות, ואפשר היה למצוא אותה כבר בשפות עתיקות כמו לימוד שפת מ"א. ישנם חלופות כמו השימוש בביטויים רגולריים או פונקציות עזר מותאמות אישית, אבל באלם, 'String.toLower' היא פונקציה פשוטה וישירה שעושה את העבודה. מאחורי הקלעים, הפונקציה חייבת לכבד כללים של השפה, כמו אותיות גדולות בשפות שונות וסימנים מיוחדים.

## ראו גם
- תיעוד הפונקציה `String.toLower` באלם: [https://package.elm-lang.org/packages/elm/core/latest/String#toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- מדריך לעבודה עם טקסט ומחרוזות בסטנדרטים שונים.
