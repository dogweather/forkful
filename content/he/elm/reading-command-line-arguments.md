---
title:    "Elm: קריאת פרמטרים שורת פקודה"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## למה:

קריאת ארגומנטי פקודת שורת הפקודה היא כלי חיוני בכתיבת תוכניות במחשב. קריאת ארגומנטים מאפשרת לתוכנית לקבל מידע חיצוני ולהתאים את הריצה שלה בהתאם. כדי להיות מפתח תוכנה אפקטיבי, חשוב לדעת כיצד לקרוא ולהשתמש בארגומנטי פקודת שורת הפקודה.

## איך לעשות:

נתחיל עם דוגמת קוד של Elm לקריאת ארגומנטי פקודת שורת הפקודה:

```Elm
import Html exposing (text)
import Platform exposing (command)

main =
  Platform.worker {init = init, update = update, subscriptions = always Sub.none}

type Msg
  = GotArguments (List String)
  | NoArguments

type alias Model =
  { arguments : List String
  , output : String
  }

init : (Model, Cmd Msg)
init =
  ( { arguments = []
    , output = ""
    }
  , command NoArguments
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotArguments arguments ->
      ( { model | arguments = arguments, output = "Got arguments: " ++ String.join ", " arguments }
      , Cmd.none
      )
    NoArguments ->
      ( model, Cmd.execute GotArguments (List.fromArray argList) )
```

בדוגמה זו, אנחנו משתמשים בפונקציה המובנית command להפעלת פקודת sh באמצעות ארגומנטים שתוכננו מראש.

כאשר התוכנית שלנו תיפעל, היא תיקח את הארגומנטים מפקודת השורת הפקודה ותעדכן את המודל עם הארגומנטים החדשים ותציג אתם על האתר. הקוד בלשון אלמן מאפשר לנו לנהל את הארגומנטים החיצוניים ולהתאים את התוכנית שלנו בהתאם.

## העמקה:

לקרוא ולהשתמש בארגומנטי פקודת שורת הפקודה מאפשר לנו לבנות תוכניות יישומיות פורצות דרך ועקביות. בנוסף, זה גם מאפשר לנו ליצור תוכניות שקלות לתחזק ולאפשר שיתוף קבצים ותמונות חיצוניים מבלי להפחית את ביצועי התוכנית.

אנחנו