---
title:                "Elm: לבדוק אם תיקייה קיימת"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
בפוסט הזה נדבר על איך ניתן לבדוק אם תיקייה קיימת בשפת אלם. נלמד על כיצד הוא מתבצע ונכיר כמה דוגמאות כדי להבין אותו בצורה נכונה. 

## איך לעשות זאת
נדגים כאן כמה דוגמאות של קוד באלם כדי להראות איך לבדוק אם תיקייה קיימת. תכלית הקוד הוא לחזור עם ערך בוליאני, טרו אם התיקייה קיימת ופולס אם היא אינה קיימת. 

```Elm
-- קוד דוגמה עם תיקייה קיימת
directoryExists : String -> Bool
directoryExists path =
    True

-- תוצאה: טרו
```

```Elm
-- קוד דוגמה ללא תיקייה קיימת
directoryExists : String -> Bool
directoryExists path =
    False

-- תוצאה: פולס
```

אנו ניתן גם להשתמש בחבילת [elm-explorations/filesystem](https://package.elm-lang.org/packages/elm-explorations/filesystem/latest/) כדי לבדוק תיקייה קיימת במערכת הקבצים של המחשב. הנה דוגמא עם שימוש בחבילה זו:

```Elm
import FileSystem
import Result

directoryExists : String -> Cmd msg
directoryExists path =
    FileSystem.access (FileSystem.file path) FileSystem.canRead ()
        |> Task.perform (always <| Result.withDefault False) identity

main =
    directoryExists "C:\\Users\\Example\\MyFolder"

-- תוצאה: טרו - אם התיקייה קיימת
-- תוצאה: פולס - אם התיקייה לא קיימת
```

## להתקן עומק
מורכבות נוספת יכולה לתת מעט אור על הנושא. כאשר אנו בודקים אם תיקייה קיימת בשפת אלם, אנו מבצעים בדיקת קיומה רק בשלב הבדיקה. זה אומר שאם התיקייה מחולקת או מוסרת לתוכנית, ניתן לבצע שינויים לאחר מכן. זהו הקצה של משמעויות. כמו כן, כאשר אנו משתמשים בחבילת [elm-explorations/filesystem](https://package.elm-lang.org/packages/elm-explorations/filesystem/latest/), יש לנו גם את היכולת לבדוק אם תיקייה היא בפ