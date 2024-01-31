---
title:                "כתיבת בדיקות"
date:                  2024-01-19
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת טסטים זה פעולה שבה אנחנו בונים בדיקות אוטומטיות לקוד שלנו. אנחנו עושים את זה כדי לוודא שהקוד עובד כמתוכנן ולזהות בעיות לפני שהן מתפשטות והופכות מסובכות.

## איך לעשות:
```Elm
import Expect
import Test exposing (..)
import Test.Runner.Node

suite : Test
suite =
    describe "טסטים בסיסיים"
        [ test "בדיקת שוויון" <| \_ ->
            Expect.equal 4 (2 + 2)
        , test "בדיקת רשימות" <| \_ ->
            Expect.equal [1, 2, 3] (List.sort [3, 2, 1])
        ]

-- ריצת הטסטים דרך Node.js
main : Program () () ()
main =
    Test.Runner.Node.run suite
```
הפלט יהיה דוח הטסטים שמראה אם הטסטים עברו בהצלחה.

## עיון מעמיק
כתיבת טסטים התפתחה לאורך השנים והיום יש גישות שונות כמו TDD (פיתוח דרך טסטים) ו-BDD (פיתוח נובע מתכנון התנהגות). ב-Elm, כתיבת טסטים נעשית על ידי ספריות כגון `elm-test`, שמספקת ממשק לכתיבת טסטים יחידתיים ולאינטגרציה שלהם בקוד. כתיבת טסטים ב-Elm יכולה להיות פשוטה ועוצמתית במיוחד בזכות שפה פונקציונלית וסטטית.

## ראה גם
- [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/) - הספרייה הרשמית לטסטים ב-Elm.
- [Test Driven Development](https://martinfowler.com/bliki/TestDrivenDevelopment.html) - הסבר על פיתוח דרך טסטים על ידי Martin Fowler.
