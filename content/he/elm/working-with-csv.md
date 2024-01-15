---
title:                "עבודה עם קובץ csv"
html_title:           "Elm: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

CSV הוא פורמט תיק מידע נפוץ שמשמש לאחסון והעברת נתונים בין יישומים שונים. בעזרת האפשרויות החדשות של Elm לעזור לנו לעבוד עם קבצי CSV בקלות ויעילות רבה, ולכן ניתן להסיק כי עבודה עם CSV עם Elm היא עוד פרויקט מאוד מועיל עבור מתכנתים.

## איך לעשות זאת

כדי להתחיל לעבוד עם CSV ב Elm, נצטרך כמה דברים כמו חבילת csv וחבילת http אך אחרי הטעינה נוכל להתחיל לקרוא קבצי CSV ולייצא אותם למבני נתונים שונים כגון רשימות או רשימות משונות.

```Elm
import Html
import Csv
import Http

type alias User = { name : String, age : Int }  -- יצירת מבנה הנתונים למשתמש

readUsersResponse : Http.Response (List Csv.Decoded.Csv) -> Html.Html msg
readUsersResponse response =  -- קבלת נתונים משרת HTTP והמרתם למבנה נתונים מתאים
    case response of
        Http.BadUrl err ->
            Html.text err
        Http.Timeout ->
            Html.text "Request timed out"
        Http.NetworkError ->
            Html.text "Connection error"
        Http.BadStatus err firstHeaders ->
            Html.p [] [Html.text "Got bad response: ", Html.text <| toString err]
        Http.GoodStatus _ _ ->
            case responseHeaders of
                Nothing ->
                    Html.text "No content-length specified"
                Just csvHeaders ->
                    case Http.stringHeader "content-type" csvHeaders of
                        Nothing ->
                            Html.text "No content-type specified"
                        Just maybeCsv ->
                            decodeUsers (Csv.decode (List.head maybeCsv)) -- המרה של נתוני csv לעזרת התכנית הבאה
                                |> renderUsers

decodeUsers : Csv.Decoded.Decoder List Csv.Deserialize.Errors -> List User
decodeUsers csvDecoder =
    List.map (\csvResult -> -- עבור כל שורת שיכון, הפעל את Csv.Decode.decode כדי לבדוק את הנתונים של המשתמש
        case Csv.Decode.decode <| CsvDecode.list Csc.Decode.string csvResult of
            Ok [name, age] ->
                { name = name, age = String.toInt age }
                    |> Maybe.map User
                    |> Maybe.withDefault { name = "", age = Nothing }
            _ ->
                { name = "", age = Nothing }
        )
        <| Maybe.withDefault []

renderUsers : List User -> List Html.Html msg
renderUsers users = -- יצירת רשימת להציג את נתוני המשתמש לעזרת HTML
    List.map
        (\user ->
            Html.ul []
                [Html.li [] [Html.text <| "Name: " ++ user.name]
                ,Html.li [] [Html.text <| "Age: " ++ Maybe.withDefault "N/A" (Maybe.map toString user.age)]
                ]
        )
        users
```

## חפירה עמוקה

בעזרת חבילת Csv