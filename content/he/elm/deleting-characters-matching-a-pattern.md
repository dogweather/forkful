---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elm: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים המתאימים לתבנית היא פעולה שמשמשת למחיקת תווים מסוימים מהטקסט שנמצא בתוך מחרוזת. מתחתניטים מבצעים פעולה זו בכדי להסיר תווים כפולים, קטעי טקסט רצויים או כל תבנית אחרת שמתאימה למקרה שהם מתחילנים לפעולה זו.

## איך לעשות:
ניתן לכתוב קוד כדי למחוק תווים המתאימים לתבנית, ולאחר מכן להדפיס את הטקסט הנותר באמצעות חיבור בין המחרוזות הנותרות. הנה כמה דוגמאות:

```Elm
deletePattern : String -> (Char -> Bool) -> String
deletePattern str pattern = List.filter (\ch -> not (pattern ch)) str

output = deletePattern "זה טקסט לדוגמה" (/= 'כ')

-- פלט: "זה טקסט לדוגמה"
```

```Elm
deleteRepeated : String -> String
deleteRepeated str = String.foldl (\acc ch -> if ch == acc then acc else acc ++ String.fromChar ch) "" str

output = deleteRepeated "מתחתניטם"

-- פלט: "מתחניםם"
```

```Elm
deleteDigits : String -> String
deleteDigits str = String.filter (\ch -> not (ch >= '0' && ch <= '9')) str

output = deleteDigits "המחרוזת 123 כוללת מספרים 4 כמו 5 ו- 6"

-- פלט: "המחרוזת  כוללת מס פרים כמו ו- "
```

## העמקה:
ידוע שפעולת מחיקת תווים המתאימים לתבנית היא חלק מכלי עורך טקסט רחב מתחומים. פופולאריות הפעולה חלקית בה נשתמשת וגם נצפה בשפות תכנות שונות. מהות הידועה שמחיקת התווים המשימים לתבנית היא חלק חיוני מהתהליך של בקרת תווים אוטומטית.

## ראה גם:
[מדריך רשמי של מחרוזות בשפת Elm](https://guide.elm-lang.org/strings/)