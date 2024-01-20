---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Elm: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת היא פעולה של מרשם את המיקום של תיקייה ובדיקה אם היא קיימת במערכת. בודקים תיקיות כדי למנוע שגיאות קריאה/כתיבה.

## כיצד ל:
בחלק זה, אנו נציג את הקוד הדרוש לבדיקה אם תיקייה קיימת באלם. אנא שים לב שאלם אינו מספק דרך ישירה לבדוק את זה, אז אנו נוכל להשתמש ב- JS interop.
שים לב שזה יעבוד רק באמצעים של סביבת דפדפן.

```Elm
port module Main exposing (..)
import Html

port checkDirExists : String -> Cmd msg
port dirExistsResult : (Bool -> msg) -> Sub msg
```

אתה תצטרך להשלים דרך JS Interop.
```JS
app.ports.checkDirExists.subscribe(function(dirPath) {
    var fs = require('fs');
    fs.access(dirPath, function(error) {
        app.ports.dirExistsResult.send(!error);
    });
});
```

כאן אנחנו משתמשים ב- 'fs.access' של Node.js כדי לבדוק אם נתיב המכיל את התיקייה קיים.

## צלילה עמוקה
במהלך השנים, הדרך הקבועה ביותר לבדוק אם תיקייה (או קובץ) קייםת בשפות תכנות שונות היא באמצעות פונקציה שבודקת את הנתיב. אם ישנן חריגות, מסופקים להם מחזירים שגיאה, כלול ב- Node.js, fs.existsSync או fs.open.

במקרה שלנו באלם, קיימת מעטת סביב נתיבים חוצה תחום ('cross-platform path handling'). באופן טבעי, Elm מספקת API פשוטה ונקייה עבור מרבית המטרות. אבל לפעמים, אתה יכול להיות צריך להתממשק עם JavaScript דרך JS interop, כפי שהראינו למעלה.

## ראה גם
- [Elm Official Guide](https://guide.elm-lang.org/)
- [Elm Language GitHub](https://github.com/elm/compiler)
- [Working with Filepaths in Elm](https://korban.net/posts/elm/2018-11-28-working-file-paths-elm/)
- [Node.js fs documentation](https://nodejs.org/api/fs.html)