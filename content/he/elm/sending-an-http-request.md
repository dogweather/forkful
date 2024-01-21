---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T17:59:44.914017-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא פעולה שבה התכנית שלך פונה לשרת באינטרנט ומבקשת מידע או מבצעת פעולה. תכניתנים עושים זאת כדי להאבק נתונים, לשלוח נתונים או לבצע אינטראקציה עם שירותי רשת.

## איך לעשות:
ב-Elm, שליחת בקשת HTTP דורשת שימוש במודול `Http`. דוגמה בסיסית:

```Elm
import Http
import Json.Decode as Decode

type Msg
    = GotData (Result Http.Error String)

getData : Cmd Msg
getData =
    Http.get
        { url = "https://api.example.com/data"
        , expect = Http.expectString GotData
        }

type alias Model = 
    { data : String
    , error : String
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData (Ok data) ->
            ({ model | data = data }, Cmd.none)

        GotData (Err _) ->
            ({ model | error = "Failed to fetch data." }, Cmd.none)
```

תוצאת דוגמה (`Model` עדכני):

```Elm
{ data = "נתונים מהשרת", error = "" }
```

## עיון עמוק
בעבר, שליחת בקשות HTTP בכל שפת תכנות הייתה כרוכה בשימוש ב-APIs נמוכים יותר כמו XMLHttpRequest ב-JavaScript. Elm מפשטת זאת על ידי מידול של בקשות ותגובות HTTP והפיכתם לבטוחים לטיפוס. עבור ניתוח הג'ייסון (JSON), Elm משתמשת במודול `Json.Decode` כדי להבטיח שהנתונים שמתקבלים תואמים לצורה אותה אנו מצפים לה. קיימים גם שיטות אלטרנטיביות כמו GraphQL, אבל בקשת HTTP רגילה עדיין היא הכי נפוצה ומתאימה לרוב הצרכים.

## ראה גם
- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [Json.Decode documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [An introduction to Elm by Evan Czaplicki](https://guide.elm-lang.org/)
- [HTTP Basics - Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP)