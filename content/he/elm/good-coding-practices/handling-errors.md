---
date: 2024-01-26 00:52:44.938146-07:00
description: null
lastmod: 2024-02-19 22:04:58.438334
model: gpt-4-1106-preview
summary: null
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
---

{{< edit_this_page >}}

מה ולמה?
טיפול בשגיאות פירושו כתיבת קוד שיכול לצפות ולהתמודד עם דברים שלא עובדים כצפוי. מתכנתים עושים זאת כדי למנוע קריסות, לשמור על תקינות הנתונים ולספק למשתמשים מנגנון נפילה חסדי.

איך לעשות:
הפילוסופיה המרכזית של Elm היא אין יוצאות ריצה בזמן אמת. לכן, Elm מנצל את מערכת הסוגים שלו עם סוגים כמו `Maybe` ו-`Result` בשביל לטפל בשגיאות.

במצב של `Maybe`:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- כאשר תריצו את זה:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

במצב של `Result`:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- ובשימוש בזה:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

צלילה עמוקה
מערכת הסוגים של Elm היא נוקשה, וזה עוזר לאתר שגיאות מוקדם. מרבית השפות בהיסטוריה הסתמכו על יוצאות ריצה ובדיקות בזמן ריצה, אך Elm בחר בערבויות בזמן קומפילציה. אלטרנטיבות כמו `Result` מאפשרות מידע מפורט אודות השגיאה, בעוד ש-`Maybe` פשוט יותר עבור תרחישי כן-לא. טיפול בשגיאות ב-Elm מעודד פיתוח תוכנה שלוקח בחשבון את כל האפשרויות מראש, ומתחמק מהמפגע של נסיון לשכוח מקרי שגיאה.

ראו גם:
- סעיף המדריך הרשמי של Elm על טיפול בשגיאות: [טיפול בשגיאות – היכרות](https://guide.elm-lang.org/error_handling/)
- תיעוד Elm `Maybe`: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- תיעוד Elm `Result`: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
