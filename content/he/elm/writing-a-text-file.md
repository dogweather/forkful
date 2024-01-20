---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא יצירת קובץ שמכיל טקסט ברמת המערכת או האפליקציה. תוכניתנים עושים זאת כדי לשמור מידע בצורה קבועה, ללוגים, או לאחסון נתונים לשימוש חוזר.

## איך לעשות:
Elm לא מאפשר כתיבה ישירה לקובץ מהדפדפן בגלל מגבלות האבטחה. עליך לשלוח את הנתונים לשרת, או ליצור הורדת קובץ מהדפדפן. להלן דוגמא ליצירת קישור להורדת קובץ:

```Elm
module Main exposing (main)
import Browser
import Html exposing (Html, a, text, attribute)
import Html.Attributes exposing (href)

main : Html msg
main =
    let
        fileContent = "תוכן לדוגמא בעברית"
        encodedContent = "data:text/plain;charset=utf-8," ++ (encodeURIComponent fileContent)
    in
    a [ href encodedContent, attribute "download" "example.txt" ] [ text "הורד את הקובץ" ]

-- כדי לעשות encode לתוכן בעברית:
encodeURIComponent : String -> String
encodeURIComponent =
    -- פה תהיה המימוש של הפונקציה לביצוע Encode לתוכן.
```

בחר בקישור יתחיל הורדה של קובץ עם התוכן שהגדרת.

## צלילה עמוקה
בעבר, כתיבה לקבצים הייתה אפשרית בשפות תכנות המופעלות מחוץ לדפדפן עם גישה ישירה למערכת הפעלה. ב-Elm, המופעל בדפדפן, האבטחה מחייבת שימוש ב-API של הדפדפן או בקשת HTTPS לשרת. אפשרות נוספת לכתיבת קבצים היא על ידי השתמשות ב-LocalStorage או IndexedDB, אך זה לא כולל יצירת קבצי טקסט פיזיים ותלוי בדפדפן.

## ראו גם
- [Elm File](https://package.elm-lang.org/packages/elm/file/latest/) - משפחת חבילות לעבודה עם קבצים ב-Elm.
- [MDN Web Docs - Using files from web applications](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications) - מדריך על כיצד לשלוט ולגשת לקבצים באפליקציות ווב.
- [Elm Guide - HTTP](https://guide.elm-lang.org/effects/http.html) - מדריך על פעולות HTTP ב-Elm, כולל שליחה וקבלה של נתונים מ/אל שרת.