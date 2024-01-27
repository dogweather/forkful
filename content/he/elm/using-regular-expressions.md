---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
רגולר אקספרשנס (Regular Expressions, או regex) זו דרך לחיפוש ותיקון דפוסים בטקסט. מתכנתים משתמשים בהם כי הם חזקים ויעילים לניתוח ועיבוד מידע.

## איך עושים את זה:
ב-Elm, נחשוף את החבילה Regex ונבדוק אם מחרוזת כוללת מספר:

```Elm
import Regex exposing (..)

hasNumber : String -> Bool
hasNumber text =
    let
        regex = regex "\\d+"
    in
    case contains regex text of
        Nothing ->
            False

        Just _ ->
            True

-- דוגמא לשימוש:
hasNumber "Elm0Programming" -- תוצאה: True
hasNumber "ElmProgramming"  -- תוצאה: False
```

## טבילה עמוקה
Regular Expressions היו סביב כבר מאז שנות ה-50 והפכו לפופולריים בתוך שפות תכנות שונות. ב-Elm, חבילת Regex אינה כלולה כברירת מחדל וצריך להתקינה באמצעות elm-package. חלופות ל-regex עשויים להיות פונקציות ספציפיות למחרוזות (כגון contains או startsWith), אבל הם לא מציעים את אותה רמת הגמישות. היישום מתבצע על ידי פענוח הדפוס למכונת מצבים דטרמיניסטית או נון-דטרמיניסטית.

## ראו גם
- [Elm Regex package](http://package.elm-lang.org/packages/elm/regex/latest)
- [Learn Regex](https://regexone.com/)
- [Regex Tester and Debugger](https://regex101.com/)
