---
title:                "הפיכת מחרוזת לאותיות ראשיות"
html_title:           "Elm: הפיכת מחרוזת לאותיות ראשיות"
simple_title:         "הפיכת מחרוזת לאותיות ראשיות"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הרישום של מחרוזת הוא פעולה שבה כל האותיות במחרוזת הופכות לאותיות גדולות. מתכנתים מעלים מחרוזות להודעות משתמש או שם משתמש עצמי, לאיתור טקסט רגיש למקרה או למניעת טעויות בעת הזנת נתונים.

## איך ליצור:
ב-Elm, אפשר להרים את האותיות במחרוזת באמצעות מסיבי ספריית ה-string. לדוגמה:

```Elm
import String

capitalize : String -> String
capitalize str =
    String.toUpper str

main =
    print (capitalize "hello, world!")
```

הפלט המצופה הוא: "HELLO, WORLD!"

## צלילה עמוקה:
היסטורית, הרישום של מחרוזת היה משימה שנדרשה מניפולציות של מערכים של תווים. אך בשפות ברמה גבוהה כמו Elm, זה פשוט מתוך גישת ה-String API. נוסף על זה, יש אפשרויות אחרות ב-Elm שבהם אתה יכול להמיר את כל האותיות לאותיות קטנות, או סינגולרית את האות הראשונה של מחרוזת.

## לגיפוי:
תוכל לחפש מידע נוסף בנושא זה בדפים הבאים:
- Elm String Module Documentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)