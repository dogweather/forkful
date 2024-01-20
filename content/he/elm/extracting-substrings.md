---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חליפת substrings היא הפעולה של רמת ה-string בה אנחנו הופכים כחלק ממחרוזת למחרוזת נפרדת. זה שימושי כאשר נדרשים חלקים מסוימים ממחרוזת, דבר המאפשר חילוף, ניתוח ואינטרפרטציה של אותם חלקים.

## איך ל:
```Elm
substring : Int -> Int -> String -> Maybe String
substring start end string =
    if start >= 0 && end <= String.length string && start <= end then
        Just (String.slice start end string)
    else
        Nothing
```
קוד זה מאמץ את תפקידו של מחרוזת `substring` ב- Elm. הוא מחזיר `Just substring`, ואם התנאים לא מתקיימים - `Nothing`.

## צלילה מעמיקה
(1) הפונקציה `substring` ב- Elm אף פעם לא הייתה חלק בשפה, כי היא אפשרית למימוש בצורה פשוטה באמצעות פעולה של `slice`.
(2) Elm קונצנטרי בביטחון הקוד, ולכן הוא מבקש מהמתכנת שיבדוק את תנאים קדם מקדים.
(3) `slice` מומש ב- JavaScript שעליו מבוסס Elm, והוא משתמש ב-JavaScript native `substring` אם התנאים מתקיימים.

## לראות גם
1. דוקומנטציה ל- [`String`](https://package.elm-lang.org/packages/elm/core/latest/String): מאגר הפקדים המלא של אייל.
2. [איך להשתמש להבין את `Maybe`](https://guide.elm-lang.org/error_handling/maybe.html): מדריך לשימוש בהכנות `Maybe` כחלק מהוא איילת.
3. [`String.slice`](https://package.elm-lang.org/packages/elm/core/latest/String#slice): את הדוקומנטציה לפעולה `slice`.