---
date: 2024-01-20 14:56:41.388272-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05EA \u05E7\u05D9\u05D5\u05DD \u05EA\u05D9\
  \u05E7\u05D9\u05D9\u05D4 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\
  \u05D1\u05D4 \u05D0\u05E0\u05D5 \u05DE\u05D5\u05D5\u05D3\u05D0\u05D9\u05DD \u05D0\
  \u05DD \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA\
  \ \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\
  \u05D1\u05E6\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D9\u05DE\u05E0\
  \u05E2 \u05DE\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D1\u05D6\u05DE\u05DF \u05E8\
  \u05D9\u05E6\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D5\u05DC\u05D5\
  \u05D5\u05D3\u05D0 \u05E9\u05D4\u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD\u2026"
lastmod: 2024-02-19 22:04:58.449923
summary: "\u05D1\u05D3\u05D9\u05E7\u05EA \u05E7\u05D9\u05D5\u05DD \u05EA\u05D9\u05E7\
  \u05D9\u05D9\u05D4 \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\
  \u05D4 \u05D0\u05E0\u05D5 \u05DE\u05D5\u05D5\u05D3\u05D0\u05D9\u05DD \u05D0\u05DD\
  \ \u05EA\u05D9\u05E7\u05D9\u05D9\u05D4 \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA \u05E7\
  \u05D9\u05D9\u05DE\u05EA \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\
  \u05E6\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D9\u05DE\u05E0\u05E2\
  \ \u05DE\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D1\u05D6\u05DE\u05DF \u05E8\u05D9\
  \u05E6\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05D5\u05DC\u05D5\u05D5\
  \u05D3\u05D0 \u05E9\u05D4\u05EA\u05D4\u05DC\u05D9\u05DB\u05D9\u05DD\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05EA\u05D9\u05E7\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקת קיום תיקייה היא פעולה שבה אנו מוודאים אם תיקייה מסוימת קיימת במערכת הקבצים. תכנתים עושים זאת כדי להימנע משגיאות בזמן ריצת התוכנית ולוודא שהתהליכים של שמירת וקריאת קבצים יוכלו לבוצע.

## איך לעשות:
Elm לא שולט במערכת הקבצים מאחר והוא שפת צד לקוח ליצירת אפליקציות ווב. לכן, אין להשתמש ב-Elm כדי לבדוק קיום תיקייה בשרת או במחשב האישי. במקום זאת, נעשה שימוש ב-JavaScript דרך `ports` לביצוע הבדיקה ולהעביר את התוצאה ל-Elm.

```Elm
port module Main exposing (..)

-- Define a port to send directory check requests to JavaScript
port checkDirExists : String -> Cmd msg

-- A message type for directory check responses
type Msg
    = DirExistsResult Bool

-- Subscribe to the directory check responses from JavaScript
port dirExistsResponse : (Bool -> msg) -> Sub msg

-- Update function to handle the response
update : Msg -> Model -> (Model, Cmd msg)
update (DirExistsResult exists) model =
    ({ model | dirExists = exists }, Cmd.none)
```

בצד JavaScript נראה משהו כמו זה:

```javascript
// Assume the Elm app is initialized with the name 'app'

// Listen for a directory check request from Elm
app.ports.checkDirExists.subscribe(function(dir) {
    // Perform the actual check using Node.js or another server-side solution
    const exists = fs.existsSync(dir);
  
    // Send the result back to Elm
    app.ports.dirExistsResponse.send(exists);
});
```

## ניפוח עמוק:
בעבר, לפני גירסאות מודרניות של שפות פרונט-אנד כמו Elm, גישה למערכת הקבצים הייתה לרוב באמצעות שפת השרת כמו PHP, Ruby או Node.js. הצורך בשילוב של Elm עם עולם JavaScript מתגלה כאן - מתן אפשרות להשתמש ביכולות המגוונות של JavaScript וה-ecosystem שלו, תוך שילוב עם הטהרות, האנטי-פרגמיניות, והעצמתיות של Elm. האלטרנטיבה לשימוש ב-`ports` הייתה לרוב הפעלת שרת עם API שהייתה משוחחת עם הקוד ב-Elm בדרכים אחרות, אבל `ports` מאפשרת דיאלוג קל ויעיל בין Elm ל-JavaScript.

## ראה גם:
- [Elm Ports Documentation](https://guide.elm-lang.org/interop/ports.html)
- [Node.js File System Module](https://nodejs.org/api/fs.html#fs_file_system)
