---
title:                "איתר תווים מאותיות גדולות"
html_title:           "Elm: איתר תווים מאותיות גדולות"
simple_title:         "איתר תווים מאותיות גדולות"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
בכדי לכתוב תכניות ב-Elm, הנם נדרשים לכתוב שפה שכוללת מעט קולונים וסימני שאלה. אחד הדוגמאות הכי פשוטות של זה הוא שימוש משפחה בלשימוש ב "הגדולה".

## כיצד לעשות
כדי לכתוב מחרבני מלל ב-Elm, עליך להשתמש בפקודה ה "capitalize". לדוגמא:

```Elm
capitalize "אל יהיה דבר כזה"
```
הפלט הוא:
```Elm
"אל יהיה דבר כזה"
```

## התעמקות
הפונקציה "capitalize" מאפשרת לנו לכתוב קוד פשוט יותר על מנת להמיר את הטקסט לאותיות גדולות. היא משתמשת בפונקציות תמיכה כגון "isUpper" על מנת לקבוע האם אותיות הן גדולות או לא, ומשתנה ה -"toUpper" על ידי כיוון תוים לאותיות גדולות.

## ראה גם
למד עוד על שפת התכנות Elm כאן:
- https://guide.elm-lang.org/
- https://www.elm-tutorial.org/