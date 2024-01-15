---
title:                "הורדת דף אינטרנט"
html_title:           "Elm: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מדוע

הורדת עמודי אינטרנט היא פעולה חשובה כאשר מתכנתים רוצים לגשת למידע מעניין ומגוון ברחבי האינטרנט. פעולה זו מאפשרת לקבל נתונים לתוך אפליקציות וליצור חיבור ישיר למקור המידע.

## איך לעשות

כדי להוריד עמוד אינטרנט בעזרת Elm, ניתן להשתמש בפונקציית `Http.get` ולתת לה את כתובת ה-URL של העמוד כפרמטר. לדוגמה:

```Elm
import Html exposing (div)
import Http

type Msg = GotPage (Http.Result String)  -- תיאור הודעה מהאפליקציה

getUrl : String  -- קבלת כתובת ה-URL מהמשתמש או ממשתנה נמצא
getUrl =
    "https://www.example.com"  -- כתובת ה-URL של העמוד

downloadPage : Cmd Msg  -- פעולה להורדת העמוד
downloadPage =
    Http.get
        { url = getUrl,
          expect = Http.expectString GotPage }

main : Program Never () Msg
main =
    Html.program
        { init = getUrl,
          view = view,
          update = update,
          subscriptions = always Sub.none }

-- פונקציית תצוגה של חלונית HTML פשוטה
view : String -> Html Msg
view page =
    div [] [ text page ]

-- טיפול בהודעה המגיעה מהאפליקציה
update : Msg -> String -> (String, Cmd Msg)
update gotPage page =
    case gotPage of
        GotPage result ->
            case result of
                Err error ->
                    ("Unable to download page", Cmd.none)  -- אם הופיעה שגיאה

                Ok page ->
                    (page, Cmd.none)  -- אם ההורדה הצליחה

```

התוכנית תציג בחלון ה-HTML את העמוד שהורד. ניתן להשתמש בפונקציית `Http.bytesGet` כדי להוריד תמונות או קבצים אחרים מהעמוד.

## Deep Dive

במקרים מסוימים, ייתכן שתהליך ההורדה יהיה מורכב יותר וידרוש מחלקת עיבוד נספחת. במקרים אלו, ניתן להשתמש בפונקציית `Http.send` עם פרמטרי `Bytes` ולבנות מחלקה נספחת לעיבוד הנתונים המורכבים. ניתן למצוא דוגמאות של זה במאמרי העזר של Elm.

## ראה