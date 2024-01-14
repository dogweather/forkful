---
title:                "Elm: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

אלמה היא שפת תכנות פונקציונלית טהורה שמיועדת לכתיבת יישומי אינטרנט מודרניים ומתקדמים. היכולת של אלמה להתגבר על בעיות כמו באגים והקלות שלה לתחזוק יוצרים סביבה אידיאלית לכתיבת תוכניות מתקדמות כגון הורדת עמודי אינטרנט.

## איך לעשות

החל מגרסה 0.19, אלמה מציגה ספרייה מובנית בשם `Http` שמאפשרת התקשרות עם שרתים חיצוניים והורדת נתונים מהם. למעשה, הורדת עמודי אינטרנט היא עבודה פשוטה מאוד באלמה, כמו שניתן לראות בדוגמא הבאה:

```Elm
import Http
import Html exposing (..)

type Msg = PageLoaded (Result Http.Error String)

type alias Model = { pageContent : String }

pageRequest : Http.Request
pageRequest =
    Http.get "https://www.example.com/page"

update : Msg -> Model -> Model
update msg model =
    case msg of
        PageLoaded result ->
            case result of
                Ok content ->
                    { model | pageContent = content }

                Err _ ->
                    model

subscriptions : Model -> Sub Msg
subscriptions model =
    Http.send PageLoaded pageRequest

view : Model -> Html Msg
view model =
    h1 [] [ text model.pageContent ]
```

 כאשר תיפתח עמוד זה בדפדפן, אלמה יוריד את תוכן העמוד ויציג אותו בתוך הכותרת הראשית של העמוד. כמו כן, אלמה מאפשרת הורדת נתונים כלליים, לא רק עמודי אינטרנט, ולהתאמה אישית של הבקשות לכל מטרה.

## צלילה מעמיקה

כמו שראינו בדוגמא, השימוש בספריית `Http` של אלמה הוא פשוט ויעיל. כמו כן, ניתן להתאים את הבקשות לכל מטרה ולהתאמה אישית מלאה, בעזרת הפונקציות המובנות של `Http` כמו `post`, `put` ו-`delete`. כמו כן, ניתן גם להשתמש בפונקציות נמצאות בספריות חיצוניות כדי לתמש בתכונות מתקדמות יותר, כגון