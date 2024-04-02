---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:33.565118-07:00
description: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05D1-Elm \u05DB\u05D5\u05DC\u05DC\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\
  \u05DB\u05D9 HTML. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05DE\u05DE\
  \u05E9\u05E7 \u05E2\u05DD \u05EA\u05D5\u05DB\u05DF \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8\u05D9 \u05D0\u05D5 API-\u05D9\u05DD \u05E9\u05DE\u05D7\u05D6\u05D9\u05E8\
  \u05D9\u05DD HTML, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05D9\u05E6\
  \u05D9\u05E8\u05D4 \u05E9\u05DC \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\
  \u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.198632-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05D1-Elm \u05DB\u05D5\u05DC\u05DC\
  \ \u05D7\u05D9\u05DC\u05D5\u05E5 \u05DE\u05D9\u05D3\u05E2 \u05DE\u05DE\u05E1\u05DE\
  \u05DB\u05D9 HTML. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05DE\u05DE\
  \u05E9\u05E7 \u05E2\u05DD \u05EA\u05D5\u05DB\u05DF \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8\u05D9 \u05D0\u05D5 API-\u05D9\u05DD \u05E9\u05DE\u05D7\u05D6\u05D9\u05E8\
  \u05D9\u05DD HTML, \u05DE\u05D4 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05D9\u05E6\
  \u05D9\u05E8\u05D4 \u05E9\u05DC \u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\u05D9\u05D5\
  \u05EA \u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

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
