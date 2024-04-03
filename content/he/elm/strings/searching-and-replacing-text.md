---
date: 2024-01-20 17:58:15.629358-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Elm, \u05D0\
  \u05E0\u05D7\u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05D4\u05E9\
  \u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\
  \u05D5\u05EA \u05E9\u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05D1\u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05EA `String` \u05DC\u05E6\u05D5\u05E8\u05DA \u05D7\u05D9\u05E4\u05D5\
  \u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05D4. \u05E0\u05E8\u05D0\u05D4 \u05D3\u05D5\
  \u05D2\u05DE\u05D4."
lastmod: '2024-03-13T22:44:39.177151-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-Elm, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\
  \u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05E9\u05DE\u05D5\u05D1\u05E0\u05D9\u05EA\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `String` \u05DC\u05E6\u05D5\u05E8\u05DA\
  \ \u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05D4."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## איך לעשות:
ב-Elm, אנחנו יכולים להשתמש בפונקציונליות שמובנית בספריית `String` לצורך חיפוש והחלפה. נראה דוגמה:

```elm
import String

replaceText : String -> String -> String -> String
replaceText toFind toReplace inText =
    String.replace toFind toReplace inText

main =
    let
        originalText = "שלום עולם"
        newText = replaceText "עולם" "world" originalText
    in
    text newText
-- תוצאה: "שלום world"
```

## עיון מעמיק:
חיפוש והחלפת טקסט היא פונקציה יסודית במרבית שפות התכנות והיא קיימת משנות ה-60. ישנם כלים רבים לחיפוש והחלפה כמו רגולר אקספרשנס ומערכת עיבוד טקסטים למשל. ב-Elm, חיפוש והחלפה מתבצעים באופן פשוט יותר, ללא תמיכה ישירה בביטויים רגולריים. זה חלק ממודל הפשטות של השפה אשר מקל על התוכניתן אך לעיתים מגביל.

## ראה גם:
- התיעוד הרשמי לספריה `String` ב-Elm: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- פוסט בלוג על עיבוד טקסט ב-Elm: https://elm-lang.org/news/patterns-in-text
- מדריך על עבודה עם מחרוזות ב-Elm: https://korban.net/elm/elm-string-handling/
