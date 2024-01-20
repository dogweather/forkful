---
title:                "מציאת אורך המחרוזת"
html_title:           "Elm: מציאת אורך המחרוזת"
simple_title:         "מציאת אורך המחרוזת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

מציאת אורך של מחרוזת היא פעולה שבה אנו מספרים האם כמה תווים יש במחרוזת. מתכנתים יעשו זאת כדי, לדוגמה, לקבוע האם מחרוזת היא ריקה או איננה ריקה, או לבצע ניתוח טקסט.

## איך ל:

ב-Haskell, אנו משתמשים בפונקציה `length` כדי למצוא את אורך מחרוזת. נסתכל על דוגמה:

```Haskell
let str = "שלום עולם"
print(length str)
```

הפלט של the הקוד הזה יהיה `9`.

## שיעור מעמיק: 
(הקשר היסטורי, אלטרנטיבות, ופרטי מימוש)

על אף שאורך מחרוזת שמעורר התלהבות מינימלית, ההגדרה שלה משחקת תפקיד מרכזי בתיאוריה של מחרוזות ובהגדרת האלגוריתמים שעובדים עמ' הן.

מה בנוגע לאלטרנטיבות? בעצם אפשר לכתוב פונקציה שלך למציאת אורך של מחרוזת:

```Haskell
stringLength :: String -> Int
stringLength [] = 0
stringLength (_:xs) = 1 + stringLength xs
```

פעולת המימוש של `length` היא win-loss - היא יעילה מבחינת זיכרון כי היא לא צריכה לעמוד על המחרוזת, אך זו קטסטרופה במחשבה אם הערך כבר נמדד, כי הפונקציה זקוקה לרוץ שוב כדי למצוא מחדש את התוצאה.

## קרא גם: 

- [Haskell Wiki - Strings](https://wiki.haskell.org/Strings)
- [StackOverflow - How is the length function implemented in Haskell?](https://stackoverflow.com/questions/23222472/how-is-the-length-function-implemented-in-haskell) - For a discussion on implementation details.