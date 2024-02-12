---
title:                "הגדלת אותיות במחרוזת"
aliases:
- /he/elm/capitalizing-a-string/
date:                  2024-02-03T19:05:25.344704-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

הפיכת מחרוזת לראשית גדולה כוללת המרה של התו הראשוני במחרוזת נתונה לאותיות רישיות, תוך שמירה על יתר התווים באותיות קטנות, לעיתים קרובות לצורך עיצוב מתוקנן או קריאות. מתכנתים ביצעו לעיתים קרובות משימה זו כדי להבטיח שהנתונים מוצגים באופן עקבי, במיוחד בממשקי משתמש או בעת עיבוד והצגת קלט משתמש.

## איך לעשות:

ב-Elm, אין פונקציה מובנית במיוחד להפיכת מחרוזות לראשית גדולה. עם זאת, אתה יכול להשיג זאת בקלות על ידי שימוש בפונקציות המודול `String` המובנות כגון `toUpper`, `toLower`, `left`, ו-`dropLeft`.

```elm
capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        String.toUpper (String.left 1 str) ++ String.toLower (String.dropLeft 1 str)

-- דוגמה לשימוש
main =
    String.toList "hello world" |> List.map capitalize |> String.join " "
    -- פלט: "Hello World"
```

לסיטואציות מורכבות יותר או אם אתה מעדיף להשתמש בספרייה המספקת דרך ישירה להפיכת מחרוזות לראשית גדולה, ייתכן שתשקול חבילה של צד שלישי כגון `elm-community/string-extra`. עם זאת, לפי העדכון האחרון שלי, האקוסיסטם של Elm מעודד טיפול במשימות כאלו באמצעות פונקציות מובנות כדי לשמור על השפה והפרויקטים רזים.

```elm
import String.Extra as StringExtra

-- במקרה שיש פונקציה `capitalize` בספריית צד שלישי
capitalizeWithLibrary : String -> String
capitalizeWithLibrary str =
    StringExtra.capitalize str

-- דוגמה לשימוש עם פונקציה היפותטית של ספרייה
main =
    "this is elm" |> capitalizeWithLibrary
    -- פלט היפותטי: "This is elm"
```

תמיד בדוק את מאגר החבילות של Elm עבור הספריות העדכניות והמועדפות ביותר למניפולציה של מחרוזות אם אתה מחפש פונקציונליות נוספת מעבר לספרייה הסטנדרטית.
