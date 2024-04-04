---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-21 21:19:20.721824-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E4\u05D9\
  \u05DC\u05D5\u05E1\u05D5\u05E4\u05D9\u05D4 \u05D4\u05DE\u05E8\u05DB\u05D6\u05D9\u05EA\
  \ \u05E9\u05DC Elm \u05D4\u05D9\u05D0 \u05D0\u05D9\u05DF \u05D7\u05E8\u05D9\u05D2\
  \u05D5\u05EA \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\u05D4. \u05DC\u05DB\u05DF\
  , Elm \u05DE\u05E0\u05E6\u05DC\u05EA \u05D0\u05EA \u05DE\u05E2\u05E8\u05DB\u05EA\
  \ \u05D4\u05D8\u05D9\u05E4\u05D5\u05E1\u05D9\u05DD \u05E9\u05DC\u05D4 \u05E2\u05DD\
  \ \u05D8\u05D9\u05E4\u05D5\u05E1\u05D9\u05DD \u05DB\u05DE\u05D5 `Maybe` \u05D5-`Result`\
  \ \u05DC\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  .\u2026"
lastmod: '2024-04-04T00:26:55.655160-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DC\u05D5\u05E1\u05D5\u05E4\u05D9\u05D4 \u05D4\u05DE\
  \u05E8\u05DB\u05D6\u05D9\u05EA \u05E9\u05DC Elm \u05D4\u05D9\u05D0 \u05D0\u05D9\u05DF\
  \ \u05D7\u05E8\u05D9\u05D2\u05D5\u05EA \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\u05E6\
  \u05D4."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## איך לעשות:
הפילוסופיה המרכזית של Elm היא אין חריגות בזמן ריצה. לכן, Elm מנצלת את מערכת הטיפוסים שלה עם טיפוסים כמו `Maybe` ו-`Result` לטיפול בשגיאות.

לסיטואציה של `Maybe`:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- כשמריצים את זה:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

לסיטואציה של `Result`:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- וכשמשתמשים בזה:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## צלילה עמוקה
מערכת הטיפוסים של Elm היא נוקשה, מה שעוזר לזהות שגיאות מוקדם. באופן היסטורי, רוב השפות הסתמכו על חריגות ובדיקות בזמן ריצה, אך Elm בחרה בהבטחות בזמן ההידור. חלופות כמו `Result` מאפשרות מידע מפורט על שגיאות, בעוד ש-`Maybe` פשוט יותר לסיטואציות של כן-לא. מנגנון טיפול השגיאות של Elm מעודד מפתחים לשקול את כל המסלולים מראש, ובכך להימנע מפיתולי המקרים של שגיאות שנשכחו.

## ראה גם:
- פרק המדריך הרשמי של Elm על טיפול בשגיאות: [טיפול בשגיאות – מבוא](https://guide.elm-lang.org/error_handling/)
- תיעוד Elm של `Maybe`: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- תיעוד Elm של `Result`: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
