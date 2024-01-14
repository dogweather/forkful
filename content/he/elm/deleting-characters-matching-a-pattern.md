---
title:                "Elm: מחיקת תווים התואמים תבנית"
simple_title:         "מחיקת תווים התואמים תבנית"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#″″ מדוע

כמו כל כתיבת קוד, בעיקרון ישנם מספר דרכים להתמודדות עם בעיות שונות. מחיקת תווים המתאימים לתבנית מבין אלו היא כלי חשוב במגוון הרחב של שפת Elm. כדי להבין לעומק את הכוח של כלי זה, נדגים כמה דוגמאות פשוטות ונעיין בשימושים נפוצים שלו.

#″″ איך לעשות

תחילה נגדיר פונקציה בשם `deleteMatchingCharacters` המקבלת שני פרמטרים: מחרוזת ותבנית. נשתמש בפונקציות כמו `toList`, `map`, ו- `filter` כדי לממש את הפונקציה.

```Elm
deleteMatchingCharacters : String -> String -> String
deleteMatchingCharacters str pattern = 
  let
    strList = String.toList str
    matchingCharacters = List.filter (\c -> c /= pattern) strList
  in
    List.map String.fromChar matchingCharacters |> String.concat
```

למשל, אם נקרא לפונקציה כך: `deleteMatchingCharacters "Hello, World!" "l"`, הפלט יהיה `Heo, Word!`.

את הפונקציה ניתן להשתמש בה במגוון רחב של תרגילים, כגון מחיקת תווים מעקב לפי תבניות (למשל, מחיקת כל הספרות או האותיות), מחיקת תווים מסוימים בתחילת ובסוף המחרוזת, ועוד.

#″″ העריכה העמוקה

בתוך הפונקציה `deleteMatchingCharacters` יש לנו משתנה `matchingCharacters` שמכיל רשימה של תווים התואמים את התבנית שנמחקים מהמחרוזת שהתקבלה כפרמטר. על מנת להבין את התהליך עומקית יותר, נחקור את הפונקציות שהשתמשנו בהן.

קוד הפונקציה `deleteMatchingCharacters` מתחיל עם השימוש בפונקציה `String.toList` שמקבלת מחרוזת וממירה אותה לרשימה של תווים. נקבל כך את המחרוזת "Hello, World!" כמו כן: `['H', 'e', 'l', 'l', 'o', ',', ' ', 'W', 'o', 'r', 'l', 'd', '!']`.

לאחר מכן