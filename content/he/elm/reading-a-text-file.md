---
title:                "קריאת קובץ טקסט"
html_title:           "Elm: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

אפר שיקום רוצה לקרוא קובץ טקסט? כדי לטעון נתונים לאינטרנט כגון גרפים או טבלאות, או להציג תכנים טקסטואליים באפליקציות שונות. קריאת קבצי טקסט היא כלי חשוב ובסיסי בפיתוח תוכנה וניתן לעשות זאת באמצעות אלם.

## איך לעשות זאת

"```Elm 
import File exposing (readFile)
import String

getFileContent : Task String ()
getFileContent =
    -- שם הקובץ שברצונכם לקרוא
    let
        fileName = "data.txt"
    in
        readFile fileName
            |> Task.map (\either ->
                case either of
                    -- כאן תוכלו לעבוד עם תוכן הקובץ
                    Ok content ->
                        content

                    Err error ->
                        "" 
                        )
```"

כאן אנחנו משתמשים בפונקציה `readFile` מהמודול `File` כדי לקרוא את הקובץ ולקבל חזרה משימה שמכילה את תוכן הקובץ. ניתן לעבוד עם תוכן הקובץ באמצעות פונקציות כמו `String.lines` ו-`String.words`.

## חפירה מעמיקה

למרבה המזל, קריאת קובץ טקסט היא פשוטה ולא מצרת הרבה מידע מתקדם. אפשר לנסות לקרוא קבצים בתבניתים שונים כגון קבצי CSV או JSON ולעבוד עם הנתונים באמצעות מודולים כמו `elm-csv` ו- `elm-json`.

## ראו גם

- [קובץ README של מודול אלם שקשור לקריאת קבצי טקסט](https://github.com/elm/file/blob/master/README.md)
- [המדריך הרשמי לשפת אלם כולל דוגמאות נוספות](https://guide.elm-lang.org/architecture/effects/file.html)