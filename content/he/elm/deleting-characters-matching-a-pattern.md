---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שמתאימים לדפוס מתיחסת לתהליך של מינימיזציה של מחרוזת על ידי הסרה של תווים מסוימים. תכנתים משתמשים בו במקרה ומחרוזת מכילה מידע מיותר או במקומות בהם מחרוזת חייבת לסבל את מספר תווים מסוים.

## איך לעשות:
להלן דוגמאות לקוד בחיפוש ומחיקת תווים בElm:

```Elm
removeChar : Char -> String -> String
removeChar c string =
    String.filter ((/=) c) string

main : Html.Html msg
main =
    let
        str = "נתונים מיותרים"
        removedStr = removeChar 'י' str
    in
    Html.text removedStr
```
לאחר יישום הקוד שלמעלה התוצאה תהיה: "נתונים מותרים" 

## צלילה עמוקה
בשפות תכנות אחרות, ניתן למנוע הסרת תווים שמתאימים לדפוס באופן ישיר על כבל חוטים. ב-Elm, צרוי לבנות את הפונקציה שלך בעזרת String.filter. זה מאפשר ל-Elm להיות קריא יותר ומונע מקרים בהם מחרוזת יכולה להיות מעובדת באופן בלתי חקיר.

כמו כן, Elm מעדיף שימוש בפונקציות כמו String.filter במקום לשלוט על מחרוזת באמצעות מדריך, מה שעשוי להוות מקור לטעויות ריצה.

## ראה גם
[Elm documentation String.filter](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
[Elm guide: Strings](https://guide.elm-lang.org/interop/strings.html)