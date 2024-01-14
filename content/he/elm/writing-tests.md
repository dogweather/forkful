---
title:    "Elm: כותבים בדיקות"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-tests.md"
---

{{< edit_this_page >}}

## למה

כתיבת מבחנים לתוכנתן של ישירה הוא כלי חשוב להבטיח שהקוד שנכתב עובד כמתוכנן ולמנוע באגים לפני שהם מתרחשים בסביבת ההפצה. כתיבת מבחנים גם עוזרת לפתח כישורי תכנות ולהבין את הקוד היטב.

## איך לכתוב מבחנים ב־Elm

```elm
import Test exposing (..)
import Expect
suite : Test
suite =
    describe "MyFunction"
        [ test "2 + 2 equals 4" <|
            \_ ->
                Expect.equal 4 (2 + 2)
        , test "myFunction returns the correct string" <|
            \_ ->
                let
                    expected =
                        "Hello, World!"
                in
                    Expect.equal expected (myFunction "World")
        ]
```

## הגועה מתקדמת

כתיבת מבחנים באלם משתמשת במודול Test כדי לבצע בדיקות. מודול זה מספק טיפוסים שימושיים לביצוע בדיקות כמו Expect ו־Test. מודול זה מאפשר גם לכתוב בדיקות בפורמט שונה, כגון ייבוא קבצי JSON לבדיקה ועוד.

## ראה גם

- [מסמך רשמי על מודול Test עבור אלם](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test)
- [מדריך לכתיבת מבחנים עבור אלם](https://pragmaticstudio.com/blog/2018/4/12/how-to-test-elm)