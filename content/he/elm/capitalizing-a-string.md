---
title:    "Elm: החדשנות נתמכת במחרוזת"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# למה
בכדי להציג את הטכניקה של הפקידים בקוד וכיצור של היישומים המאוחדים כל הכוח לבעיות המודולאריות המבוססות על רגל עומד

# כיצד לבצע זאת
לפני שנתחיל, נפתור תחילה את הבעיה של הכנסה של מחרוזות רגילות עם אותיות ריקות.

הנה דוגמה של כיצד ניתן לבצע זאת באמצעות קוד Elm:

```elm
module Main exposing (..)

import String

capitalize : String -> String
capitalize str =
    if String.isEmpty str then
        ""
    else
        let
            firstChar = String.toUpper (String.left 1 str)
            restOfString = String.dropLeft 1 str
        in
        firstChar ++ restOfString

main =
    capitalize "hello elm" -- Output: "Hello elm"
```

בקטע הקוד הנתון, אנחנו משתמשים בפונקציות המובנות של String שהופכות את האות הראשונה של המחרוזת לאות ריקה ומחליפה אותה באות ריקה. כך, המילה תוכל להיות משומשת בכל תוכניות הקוד העתידיות.

# טיפול עמוק
למי שמעוניין לממש את הפעולה הזו עם יותר מנסיון אחד, ניתן להשתמש בפונקציות נוספות לדוגמה, להוסיף בדיקות לפני כן ולתת אפשרות למשתמש לבחור אם להשתמש בפיצ'ר זה או לא.

כמו כן, אפשר להתחבר לפקידים מסוימים כנסח שלושת הפקידים במקום לפתיחת כל המקרה העתידי בדרכים. 

# ראה גם
- קורס Elm של אוניברסיטת סטנפורד: https://cs110.stanford.edu/2012/pdf/CS110Reader.pdf
- הקצב של העורך העובד: https://www.drivenbycode.com/programming/elm-is-the-fastest-document-editor/
- תיעוד רשמי של Elm: https://elm-lang.org/docs