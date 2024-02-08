---
title:                "בדיקה האם תיקייה קיימת"
aliases:
- he/elm/checking-if-a-directory-exists.md
date:                  2024-01-20T14:56:41.388272-07:00
simple_title:         "בדיקה האם תיקייה קיימת"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/checking-if-a-directory-exists.md"
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
