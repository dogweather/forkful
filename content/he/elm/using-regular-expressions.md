---
title:                "Elm: שימוש בביטויים רגולריים"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

בעולם התכנות, כדאי לבחור את הכלי המתאים לכל משימה. בכמה מקרים, כלים כמו תוכניות רגילות אינם מספיקים כדי לענות על כל הדרישות שלנו. כאן נכנס בתמונה ה- Regular Expressions. מתוך שימוש ב- Regular Expressions אנו יכולים לחפש, לתאר, ולהתאים מחרוזות תוכן בכללים מחשבתיים. אם אתם מעוניינים להיות עובדים מפתחים מקצועיים, ידע התעלה אחת הינו שימוש ב- Regular Expressions.

## איך להשתמש

רגילות נקודתיים היקלה נקודתיים בכל חלקי הטקסט. נכנס בשימוש כדי למצוא כולם מחרוזות מתאימות ולשנות אותן לכחתוב עליהם. נוכל לראות כמה דוגמאות של Regular Expressions בקוד של Elm תוך שימוש בקוד הזה:

```elm
-- בדיקה אם המחרוזות מדויקות
regexMatch : String -> Html msg
regexMatch str =
    if Regex.contains "hello" str then
        return div [] [ text "המחרוזת תואה ל- hello" ]
    else
        return div [] [ text "המחרוזת לא תואה ל hello" ]

-- החלפת מחרוזות
regexReplace : String -> String -> Html msg
regexReplace from to =
    let
        newString =
            Regex.replace (Regex.fromStrings from) (\_ -> to) "hello, world!"
    in
        return div [] [ text "המחרוזת החדשה היא: ", text newString ]
```

שימו לב שפונקציות המשמשות במחרוזת זוכונו יישומים במקומות שונים. בכל מצב, בכמה מקרים משמשים ב- Regex מצליחים להשתמש בהבלול. המחרוזת הנזונה על ידי משתנים עוזרכם נבחרה על מנת להשתמש בתאוריות המאסים.

## צפייה נמעצרת

כמו שצויין לפני כן, פנקציות המשמשות במחרוזת יכולות לקרוא למקרה מספרי. כאן השתמש בדגמה והדגמה, אבל אומרים, הטקסט המכו