---
title:                "Elm: מציאת אורך של מחרוזת"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה?
מציאת אורך של מחרוזת היא פעולה שימושית ונפוצה בתכנות באלם. היא מאפשרת לנו לטפל בנתונים שהוזנו על ידי המשתמש במתודה מדוייקת ונוחה.

## איך לעשות זאת?
ניתן להשתמש בפונקציית `String.length` כדי למצוא את האורך של מחרוזת ספציפית. הנה דוגמה פשוטה:

```Elm
let myString = "שלום עולם"
String.length myString -- יחזיר 9
```

ניתן גם להשתמש בפונקציה `List.length` בתוך `String.toList` כדי לקבל את אורך המחרוזת בצורה יותר מסודרת כך:

```Elm
let myString = "שלום עולם"
myString |> String.toList |> List.length -- יחזיר 9
```

## חידוש עומק
מציאת אורך של מחרוזות יכול להיות מניע לעוד פעולות חשובות כגון חיתוך, התאמה לתבנית או גיבוי של נתונים. בנוסף, כאשר משתמשים בפונקציות נמצאות כבר באלם, כמו `String.length`, יש אפשרות לחסוך זמן ומאמץ בפיתוח.

## ראה גם
- [פונקציית `String.length` באתר הרשמי של אלם](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- [מדריך ממוחשב לאלם (בעברית) על אורך מחרוזות](https://ebaytech.berlin/en/blog/elm-for-beginners-a-debugging-cheat-sheet-a0c1f87833b7/#string-length)
- [קורס לימוד אלם בעברית על מחרוזות](https://www.youtube.com/watch?v=WvDQlvT3GvI)