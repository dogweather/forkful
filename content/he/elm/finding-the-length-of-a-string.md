---
title:                "איתור אורך מחרוזת"
html_title:           "Elm: איתור אורך מחרוזת"
simple_title:         "איתור אורך מחרוזת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מדוע

למה לחפש את האורך של מחרוזת? הכרחי שאנו עובדים עם מחרוזות בקוד בכל יום. לעיתים אנו רוצים לדעת את האורך של מחרוזת מסוימת, כגון כאשר אנו רוצים לוודא שהיא לא ריקה או כאשר אנו רוצים לבדוק את אורך הקלט של משתמש. למצוא את האורך של מחרוזת הוא דבר פשוט ושימושי שיש לדעת.

## איך לעשות זאת

אפשרות קלה למצוא את האורך של מחרוזת בשפת Elm היא על ידי שימוש בפונקציה "String.length". הנה דוגמאות לשימוש בפונקציה ופלט צפוי:

```Elm
String.length "Hello" -- Output: 5
String.length "" -- Output: 0
```

ניתן גם לשלב את הפונקציה עם אינטרפולציה של מחרוזת בתוך פונקציה מאתר אחד:

```Elm
String.length (String.fromInt (List.length [1,2,3])) -- Output: 1
```

## חקירה מעמיקה

אם ברצונכם להבין יותר על פעולת הפונקציה string.length, ניתן לראות שהיא פועלת באופן דומה לפונקציה length בשפות אחרות. היא מכניסה את המחרוזת לתוך רשימה וחשבת את מספר האיברים ברשימה כדי לקבוע את אורך המחרוזת. ניתן גם ליצור פונקציה דומה באופן ידני בעזרת מבנה נתונים ולהשתמש בו על ידי עבור המחרוזת שאליה ברצונכם לחשב את האורך. 

## ראה גם

- תיעוד לפונקציה "String.length": https://package.elm-lang.org/packages/elm/core/latest/String#length
- עוד על שימוש במחרוזות בשפת Elm: https://elmprogramming.com/string-basics.html