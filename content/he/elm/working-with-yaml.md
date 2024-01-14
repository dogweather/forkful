---
title:                "Elm: עובדים עם YAML"
simple_title:         "עובדים עם YAML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

# למה

למה אנשים מתעסקים עם YAML? בקיצור, YAML היא שפת תיאור לנתונים קונפיגורציים. היא מאפשרת לנו לכתוב קבצי תצורה קריאים ונגישים בצורה פשוטה ויעילה.

# איך לעשות זאת

נתחיל עם קוד דוגמה פשוט של YAML:

```Elm
data:
    name: "John"
    age: 30
    occupation: "Developer"
```

אם נרצה לקרוא את הנתונים הללו ב-Elm, ניצור פונקציה פשוטה בשם "readYaml" שתקבל כפרמטר קובץ YAML ותחזיר נתונים בפורמט של מאפייני עץ. נשתמש בספריית [elm-yaml](https://package.elm-lang.org/packages/NoRedInk/elm-yaml/latest/) כדי לקרוא קבצי YAML ב-Elm.

```Elm
import Yaml exposing (..) 
import Result exposing (Result)

readYaml : String -> Result String Value 
readYaml filePath = 
    data 
        |> Yaml.decodeString 
        |> Result.mapError toString
```

בדוגמה שלהלן, אנחנו קוראים קובץ YAML בשם "data.yaml" ומקבלים את הנתונים בפורמט של מאפייני עץ, כך שנוכל לעבוד איתם בקלות:

```Elm
readYaml "data.yaml"
-- ~> Ok (Value.Object [ ("name", Value.String "John")
                      , ("age", Value.Number 30)
                      , ("occupation", Value.String "Developer") 
                      ])
```

# עיון מעמיק

כדי לעבוד עם YAML בצורה יעילה יותר, יש לבחור ספריית Elm נוספת שתתאים לצרכים שלינו. לדוגמה, אם אנחנו רוצים לכתוב קבצי YAML מבנה דינמיים ב-Elm, נוכל להשתמש בספריית [elm-yaml-builder](https://package.elm-lang.org/packages/jamesmacaulay/elm-yaml-builder/latest/). אם אנחנו מעוניינים ליצור ולערוך קבצי YAML בצורה יעילה יותר, הספרייה [elm-yaml-gen](https://package.elm-lang.org/packages/NoRedInk/elm-yaml-gen/latest/) תחזיר נתונים בפורמט של מבנה דינמי שאנחנו יכולים לעבוד איתו.

# ראו גם

* [ספריית elm-yaml](https://package.elm-lang.org/packages/NoRedInk/elm-yaml/latest/) - לקריאה של קבצי YAML ב