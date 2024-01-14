---
title:                "Elm: מחיקת תווים התואמים לתבנית"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

בכתיבת קוד באלם, לעיתים ייתכן שנתקלו בצורך למחוק תווים שמתאימים לתבנית מסוימת. המשימה הזו ניתנת לביצוע בכמה דרכים שונות תלויות בצרכי הקוד המיוחדים לכל מקרה. בכתבה זו נכיר שתי דרכים למחיקת תווים כאלו, ונלמד מתי כדאי להשתמש בכל דרך באופן לגימור הקוד שלנו.

## איך לעשות

למחיקת תווים ניתן להשתמש בפונקציית `String.dropWhile` שמקבלת כקלט פונקציה שבודקת אם לתו מסוים יש תבנית מסוימת ומחזירה אמת או שקר לפי הצורך. לדוגמה:

```Elm
nameContainsPattern : String -> Bool
nameContainsPattern name =
    String.contains "elm" name

removePattern : String -> String
removePattern str =
    String.dropWhile nameContainsPattern str
```

כמו כן, ניתן להשתמש בפונקציית `String.replace` שמחליפה תבנית מסוימת בתווים אחרים. לדוגמה:

```Elm
removePattern : String -> String
removePattern str =
    String.replace "elm" "ELM" str
```

אם התבנית מכילה פסיקים או תווים מיוחדים, ניתן להשתמש בפונקציית `String.filter` שמסננת את התווים שאינם עונים על התבנית הכווינת. לדוגמה:

```Elm
removeSpecialChars : String -> String
removeSpecialChars str =
    String.filter (\char -> Char.isAlpha char || Char.isSpace char) str
```

אלו רק כמה דוגמאות מתוך הרבה דרכים אפשריות למחיקת תווים מתאימים לתבנית. מומלץ לחקור ולנסות עם פונקציות נוספות לפי הצורך של כל קוד מסוים.

## מעמקים

מחיקת תווים מתאימים לתבנית באלם ניתנת לביצוע בכמה דרכים, ולכן חשוב לבחור את הדרך המתאימה ביותר לצרכי הקוד שלנו. לדוגמה, אם יש לנו רשימת מחרוזות עם כמ