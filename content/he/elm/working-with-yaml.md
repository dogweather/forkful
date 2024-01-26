---
title:                "עבודה עם YAML"
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## מה ולמה?
YAML הוא פורמט המחליף קבצים עבור תצורה ונתונים. תוכניתנים משתמשים בו כי הוא קריא, נתמך רחב, ונוח להשתלבות עם שפות תכנות.

## איך לעשות:
Elm לא תומך ב-YAML באופן ישיר. נדרש שימוש ב-JS לפענוח ועטיפה ב-`Json.Decode` לטיפול בנתונים.

```Elm
port module Main exposing (..)

-- יצוא פורט לקבלת נתונים מקובץ YAML ב-JS
port getYaml : () -> Cmd msg

-- פורט לארוע המבקש לפענח את הנתונים מ-YAML
port decodeYaml : (String -> msg) -> Sub msg

-- ייבוא דקודרים שלנו
import Json.Decode as Decode

type alias MyData =
    { name : String
    , age : Int
    }

-- דקודר לנתונים שלנו
myDataDecoder : Decode.Decoder MyData
myDataDecoder =
    Decode.map2 MyData
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

-- פונקציית עזר לפענוח ה-YAML ל-Elm
decodeYamlString : String -> Result Decode.Error MyData
decodeYamlString yamlString =
    Decode.decodeString myDataDecoder yamlString

-- השימוש בפורט כדי לבקש את הנתונים
requestYaml : Cmd msg
requestYaml =
    getYaml ()
```

פלט לדוגמה (JS):
```JavaScript
app.ports.decodeYaml.send('name: "Alice"\nage: 30');
```

## עיון מעמיק:
YAML (YAML Ain't Markup Language) נוצר ב-2001. אף על פי ש-JSON ו-XML שמשימוש נרחב יותר, YAML הוא בחירה מצוינת לקבצי תצורה בזכות הקריאות שלו. ב-Elm, אי-התמיכה ישירה מאלצת שימוש ב-JavaScript לפענוח, דבר שמוסיף סיבוכיות אך מאפשר גמישות באינטראקציה עם טכנולוגיות חיצוניות.

## ראו גם:
- [YAML חיצוני ל-Elm](https://github.com/terezka/yaml) - ספריה שתעזור לכם להתמודד עם YAML בתוך יישומי Elm.
- [JSON Decode רשמי של Elm](https://package.elm-lang.org/packages/elm/json/latest/) - מידע נוסף על הדקודרים ב-Elm שתזדקקו להם בפענוח.
- [מדריך לשימוש ב-Ports ב-Elm](https://guide.elm-lang.org/interop/ports.html) - רכיבי יסוד להבנת איך לשלב קוד JavaScript עם היישום Elm שלכם.
