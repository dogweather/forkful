---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
כאשר אנו מתחילים פרויקט חדש בתכנות, אנו בעצם בונים יישום מאפס, לחפש פתרונות לבעיה ספציפית. מתכנתים עושים את זה כאשר הם מנסים ליצור מוצר שיש מחסור בו בשוק, או כאשר הם מחפשים לחדד הבנה של השפה והמנגנון מאחורי הקוד.

## איך לעשות:
נכתוב קוד בססי של אפליקציה אלם בעזרת מבנה של פרויקט מאורגן.
```Elm
module Main exposing (..)

import Html exposing (div, text, beginnerProgram)

type alias Model = String

init : Model
init =
    "Hello, World!"

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
    model

view : Model -> Html.Html Msg
view model =
    div [] [ text model ]

main =
    beginnerProgram { model = init, view = view, update = update }
```
כאשר אנו מריצים את הפרויקט, זה יחזיר “Hello, World!” בדפדפן.

## צלילה מעמיקה:
השפה הפונקטיונאלית של אלם נוצרה ב2012 על ידי Evan Czaplicki כחלק מהמחקר שלו לתואר שני. בערוץ של השפה יש פוקוס על יציבות ובטיחות, ולכן היא מספקת הגנה מלאה ממחסורים רגילים של JavaScript כמו יוצאים למנות. יתר על כן, ישנן אלטרנטיבות כמו PureScript ו ReasonML אשר מספקות תכנות פונקציונאלי באותה מידה אך עם שונות במינוח ובעיצוב של השפה.

## ראה גם:
1. [תיעוד האלם הרשמי](https://package.elm-lang.org/packages/elm/core/latest/)
3. [הספר "Programming Elm: Build Safe, Sane, and Maintainable Front-End Applications"](https://pragprog.com/titles/jfelm/programming-elm/)