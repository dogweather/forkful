---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
בעבודה עם JSON אנחנו מתעסקים בתיעוד ופרשנות של נתונים מובנים. זה חשוב בשביל שיתוף נתונים בין שרתים ללקוחות, אפליקציות ומערכות.

## How to: (איך לעשות:)
קוד ב-Elm לעבודה עם JSON:

```Elm
import Json.Decode exposing (Decoder, string, int, list, field)
import Json.Encode exposing (object, string, int)

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decoder User
userDecoder =
    field "id" int
        |> Json.Decode.andThen (\id ->
            field "name" string
                |> Json.Decode.map (\name ->
                    { id = id, name = name }
                )
           )

encodeUser : User -> Json.Encode.Value
encodeUser user =
    object
        [ ( "id", int user.id )
        , ( "name", string user.name )
        ]

-- דוגמא לשימוש

decodedUser : Result String User
decodedUser = Json.Decode.decodeString userDecoder "{\"id\":1,\"name\":\"Alice\"}"

encodedUser : Json.Encode.Value
encodedUser = encodeUser { id = 2, name = "Bob" }
```

תוצאות דוגמא:

```Elm
-- פלט של decodedUser
Ok { id = 1, name = "Alice" }

-- פלט של encodedUser
{"id":2,"name":"Bob"}
```

## Deep Dive (עומק התהום):
JSON (JavaScript Object Notation) הוא פורמט תקשורת נתונים שהחל את דרכו ב-JavaScript אבל הפך לשפה רחבה בעולם התכנות. ב-Elm, `Json.Decode` ו-`Json.Encode` מאפשרים פרשנות ויצירה של מבנים בפורמט JSON בצורה טיפוסית. חלופות כוללות את עבודה עם XML או בינאריים כמו Protocol Buffers. האימפלמנטציה ב-Elm מבוססת על decoders וencoders שמתרגמים לוקחים ומתרגמים את הנתונים המובנים.

## See Also (ראה גם):
- [Elm JSON.Decode Documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [Elm JSON.Encode Documentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
- [JSON in Elm Guide](https://guide.elm-lang.org/interop/json.html)
