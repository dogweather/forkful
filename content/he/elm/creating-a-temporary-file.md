---
title:    "Elm: יצירת קובץ זמני"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why 
למה: רק עם 1-2 משפטים מסבירים *למה* מישהו יעסוק ביצירת קובץ זמני.

יצירת קבץ זמני נחשבת לטכניקה נפוצה בייצוג מידע זמני בסביבת תכנות. ניתן להשתמש בקבצים זמניים כדי לאחסן מידע שאינו קבוע או שנדרש לשנות באופן עקבי. לדוגמה, בבניית אפליקציית משחקים כתב לנו ניתן להשתמש בקובץ זמני כדי לשמור את הניקוד הזמני של כל משתתף.

## How To

כיצד ליצור קובץ זמני בכמה שורות קוד באלם:

```Elm
import File exposing (write)
import Http as Http
import Time
import Json.Decode exposing (decodeValue, int, list)
import Json.Encode

main =
    Http.get "http://example.com/scores" (Http.expectJson GotScores scoresDecoder)

type Msg
    = GotScores (Result Http.Error (List Score))

type alias Score =
    { name : String
    , score : Int
    , timestamp : Time.Posix
    }

scoresDecoder : Decoder (List Score)
scoresDecoder =
    list
        (decodeValue
            { name = int
            , score = int
            , timestamp =
                map Time.millisToPosix int
                -- ממיר את המנויות לזמן POSIX
            }
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotScores (Ok scores) ->
            ( { model | scores = scores }, Cmd.none )

        GotScores (Err _) -> ( model, Cmd.none )


-- כדי ליצור קובץ זמני הנתונים הנלוים מתוך רישומי הסקורים היופס כותבת לנו
main =
    case Http.sendHttp 0.5 "http://example.com/scores?a=1&b=2"
        (Json.Encode.object
            [ ("msg", "some message")
            , ("data", Json.Encode.string data)
            ]
        ) of
        Http.Error statusCode responseHeaders proxy err ->
            fail
                { statusCode = if statusCode == 0 then Nothing else Just statusCode
                , responseHeaders = responseHeaders
                , proxy = proxy
                , error = err
                }

        Http.Ok response ->
            let
                tempPath =
                    "results.json"

                body =
                    Http.responseToBody response
            in
                write tempPath body
                    |> execute
                    |> map (map (const "קובץ זמני נוצר בהצלחה"))
```

 השימוש בקוד זה מדמה איך ליצור קובץ זמני באלם באמצעות כתיבת נתונים לקובץ זמני והצגת הודעה כאשר הקובץ נוצר בהצלחה.

## Deep Dive

יצירת קובץ זמני באפליקציית אלם אינה משימה מסובכת. כ