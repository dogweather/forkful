---
title:    "Elm: קריאת ארגומנטים משורת הפקודה"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## למה

פיצ'ר קריאת ארגומנטי שורת פקודה באלם הוא חלק חשוב מהתכנן והעיבוד של קוד. הוא מאפשר לנו לקבל מידע מהמשתמש שמריץ את התוכנית שלנו. זה מאפשר לנו ליצור בעיות יעילות יותר ולהתאים את התוכנית לצרכיו של המשתמש.

## איך לעבוד עם פקודות שורת פקודה באלם

כדי לקרוא ארגומנטי שורת הפקודה באלם, נצטרך להשתמש בפונקציה מובנית שנקראת "command". ניתן להשתמש בפונקציה זו כדי לקבל את הארגומנטים כפרמטרים לתוכנית שלנו. לדוגמה:

```Elm
module Main exposing (main)

import Platform exposing (command)


-- הקבלת הארגומנטים כפרמטרים לתוכנית
main : Program Flags
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- האתחול
init : () -> (Model, Cmd Msg)
init _ =
    ({ text = "" }, Cmd.none)


-- עדכון המודל
type Msg
    = ReadCommand String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReadCommand command ->
            ({ model | text = command }, Cmd.none)


-- כפתורים לשליחת פקודות מהקליינט
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ command ReadCommand
        ]
        
        
-- הצגת המודל
type alias Flags =
    { init : () }

type alias Model =
    { text : String }

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "שורת הפקודה:" ]
        , div [] [ input [ onInput ReadCommand ] [] ]
        , div [] [ text ("הארגומנטים שקלטת: " ++ model.text) ]
        ]
```

ריצה ופלט: כאשר נכניס פקודה לתיבת הקלט ונלחץ על "Enter", נוכל לראות את הארגומנטים שנקלטו בתיבת התוכן שלנו.

## העמקה נוספת

פקודה של תוכנית אלם מכילה כל מיני ארגומנטים נוספים, כגון ארגומנטים נוספים להפעלת תוכניות. ניתן למצוא את כל הפקודות הללו בקובץ README ש