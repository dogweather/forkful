---
title:                "יצירת קובץ זמני"
aliases: - /he/elm/creating-a-temporary-file.md
date:                  2024-01-20T17:40:26.827825-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא פעולה שבה אנו מייצרים קובץ שתכליתו להימחק לאחר שימוש קצר. תוכניתנים עושים זאת לטובת בדיקות, אחסון נתונים חולפים, והגנה על פרטיות.

## איך לעשות:
Elm כשפת-תכנות שאינה מתמקדת בפעולות קובץ, נדרש להשתמש ב-JavaScript עבור פונקציות כאלו. נעשה שימוש ב-`ports` לקריאה וכתיבה מ/אל הקובץ הזמני. שימו לב לדוגמה הבאה:

```Elm
port module Main exposing (..)

-- Define a port to send file data to JavaScript
port requestTemporaryFile : String -> Cmd msg

-- Define a port to receive the temporary file path from JavaScript
port receiveTemporaryFilePath : (String -> msg) -> Sub msg
```

ב-JavaScript, נאזין ונגיב ל-`ports`:

```JavaScript
// JavaScript side of things to handle the Elm ports
var app = Elm.Main.init();

// Listen for a file request from Elm
app.ports.requestTemporaryFile.subscribe(function(data) {
    // Create a temporary file and return the path
    var tempFilePath = createTempFile(data);
    app.ports.receiveTemporaryFilePath.send(tempFilePath);
});

function createTempFile(data) {
    // Implementation of temporary file creation
    // Return the path of the temporary file
}
```

## עיון נוסף
קורות הדברים של קובצים זמניים הם עתיקים כמעט כמו מחשבים בעצמם. זה היה ונותר פתרון נפוץ לשימוש חולף במידע. הפתרון הנ"ל באמצעות Elm עובר דרך JavaScript מכיוון ש-Elm מיועדת לפיתוח פרונט-אנד והיא אינה מספקת גישה ישירה למערכת הקבצים.

ישנן חלופות כמו שימוש בשפות שרת עם תמיכה ישירה לגישה למערכת קבצים, כמו Node.js, PHP או Python. הדוגמה שמוצגת פה ממחישה חיבור פשוט בין ה-front-end לגבי ה-back-end.

ברמת היישום, יצירת קובץ זמני יכולה להשתמש במספר גישות, כולל יצירת שם קובץ ייחודי ודאגה למחיקתו לאחר שהשימוש פסק.

## ראו גם
- Elm `ports`: https://guide.elm-lang.org/interop/ports.html
- פונקציות ג'אווה סקריפט ליצירת קבצים זמניים: https://developer.mozilla.org/en-US/docs/Web/API/File_and_Directory_Entries_API
- HTML5 File API: https://developer.mozilla.org/en-US/docs/Web/API/File_API
