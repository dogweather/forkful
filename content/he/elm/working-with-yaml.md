---
title:                "עובדים עם YAML."
html_title:           "Elm: עובדים עם YAML."
simple_title:         "עובדים עם YAML."
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

# למה

הכתבה הזו תענה לשאלה שלך - למה להתעסק עם עבודה עם קבצי YAML באלם? או למה בכלל להשתמש בפורמט YAML? קרא ותגלה!

# איך לעבוד עם YAML ב-Elm

קוד ה- Elm מדהים וקל לקריאה, אבל ככל שפרק זמן עובר יש שליפותים של נתונים שעלינו לעבוד ולנהל מחויבת. YAML הוא פורמט פופולרי עבור שפות תיבת הכלים ומתבקש לעבוד איתו ב- Elm. במאמר הזה נראה כיצד לגשת לנתונים הממומות ב-YAML ישירות מתוך קוד ה- Elm.

בואו נתחיל עם דוגמא פשוטה של קובץ YAML כדי להתחיל את התחביר היסודי שלו:

```Elm
users:
- name: John
  age: 25
- name: Sarah
  age: 30
- name: Bob
  age: 40
```

עכשיו, ניתן לגשת לנתונים האלה מתוך קוד ה- Elm על ידי שימוש בפונקציות של מיפוי (mapping) ורדוקציה (reducing). למשל, אם אנחנו רוצים לקרוא את כל השמות של המשתמשים, נצטרך לכתוב קוד כזה:

```Elm
import YAML exposing (readString)
import Result exposing (map, withDefault)

getYAML : String -> Result String YAML.Value
getYAML yamlString =
    readString yamlString
        |> map .1

yamlToString : String -> String -> Result String String
yamlToString dataPath mainField =
  case getYAML dataPath of
    Ok yaml ->
      withDefault "No data" <| access (mainField |> String.split ".") yaml .|

    Err err ->
      Err err

mainField : String
mainField = "users"

main : Html.Html Msg
main =
  let
    users =
      yamlToString "data.yml" mainField
   in
    div [] [ text users ]
```

כאן, אנו מדפיסים את כל שמות המשתמשים מתוך קובץ YAML לתוך תיבת ה HTML שלנו.

# ייעול כלים

כעת שאנחנו כבר משתמשים בפורמט YAML בקוד ה- Elm שלנו, אנחנו יכולים להשתמש בכלים נוספים כדי להקל עלינו את העבודה. למשל, כדי להימנע מטיפול ידני בנתונים, ניתן להשתמש בספריית [elm-yaml-decode](https://package.elm-lang.org