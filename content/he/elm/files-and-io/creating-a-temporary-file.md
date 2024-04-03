---
date: 2024-01-20 17:40:26.827825-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4\
  \ \u05D0\u05E0\u05D5 \u05DE\u05D9\u05D9\u05E6\u05E8\u05D9\u05DD \u05E7\u05D5\u05D1\
  \u05E5 \u05E9\u05EA\u05DB\u05DC\u05D9\u05EA\u05D5 \u05DC\u05D4\u05D9\u05DE\u05D7\
  \u05E7 \u05DC\u05D0\u05D7\u05E8 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E7\u05E6\u05E8\
  . \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DC\u05D8\u05D5\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\
  \u05D5\u05EA, \u05D0\u05D7\u05E1\u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05D7\u05D5\u05DC\u05E4\u05D9\u05DD, \u05D5\u05D4\u05D2\u05E0\u05D4 \u05E2\u05DC\
  \ \u05E4\u05E8\u05D8\u05D9\u05D5\u05EA."
lastmod: '2024-03-13T22:44:39.234476-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\
  \u05E0\u05D5 \u05DE\u05D9\u05D9\u05E6\u05E8\u05D9\u05DD \u05E7\u05D5\u05D1\u05E5\
  \ \u05E9\u05EA\u05DB\u05DC\u05D9\u05EA\u05D5 \u05DC\u05D4\u05D9\u05DE\u05D7\u05E7\
  \ \u05DC\u05D0\u05D7\u05E8 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05E7\u05E6\u05E8."
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
weight: 21
---

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
