---
title:                "פיענוח HTML"
aliases:
- he/elm/parsing-html.md
date:                  2024-02-03T19:12:33.565118-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
פענוח HTML ב-Elm כולל חילוץ מידע ממסמכי HTML. מתכנתים עושים זאת כדי להתממשק עם תוכן אינטרנטי או API-ים שמחזירים HTML, מה שמאפשר יצירה של אפליקציות אינטרנט יותר אינטראקטיביות ודינמיות.

## איך לעשות:
ב-Elm אין ספריה מובנית לפענוח HTML ישירות בדומה לספריות ב-JavaScript או Python בשל הדגש על בטיחות טיפוסית ומניעת שגיאות בזמן ריצה. עם זאת, אפשר להשתמש בבקשות `Http` כדי לאסוף תוכן ולאחר מכן להשתמש בביטויים רגולריים או בעיבוד בצד השרת כדי לחלץ את המידע הנדרש. לפענוח HTML מורכב יותר, גישה נפוצה כוללת שימוש בשירות backend מוקדש לפענוח ה-HTML והחזרת הנתונים בפורמט שכן ניתן לעבוד איתו ישירות ב-Elm, כמו JSON.

הנה דוגמה לאיסוף תוכן HTML (בהנחה שתגובת השרת נמצאת בפורמט נקי או תוכן של תג מסוים):

```elm
import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

initialModel : Model
initialModel =
    { content = "" }

type Msg
    = Fetch
    | ReceiveContent String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch ->
            ( model
            , Http.get
                { url = "https://example.com"
                , expect = Http.expectString ReceiveContent
                }
            )

        ReceiveContent content ->
            ( { model | content = content }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    text model.content

-- נניח שהגדרות הפונקציה הראשית וההרשמות עוקבות אחר מבנה היישום הסטנדרטי של Elm.
```

לעיבוד התגובה כדי לפרש רכיבים או נתונים ספציפיים, כדאי לשקול לשלוח את תוכן ה-HTML לנקודת קצה של שרת שאתה שולט עליו, שם אפשר להשתמש בספריות שזמינות בשפות כמו JavaScript (Cheerio, Jsdom) או Python (BeautifulSoup, lxml) לצורך פענוח, ולאחר מכן להחזיר נתונים מובנים (כמו JSON) חזרה לאפליקציה שלך ב-Elm.

זכור, פענוח ישיר של HTML בקוד Elm בצד הלקוח אינו הדפוס הטיפוסי בשל מגבלות השפה והפילוסופיה לעודד הפרדה ברורה בין איסוף תוכן לבין עיבוד תוכן. ארכיטקטורת Elm נוטה לעיבוד נתונים בפורמט בטוח וניתן לחיזוי יותר כמו JSON.
