---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:47.831871-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD YAML \u05D1-Elm, \u05D1\
  \u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05D9\u05E9 \u05E6\u05D5\u05E8\u05DA \u05DC\
  \u05D4\u05DE\u05D9\u05E8 YAML \u05DC-JSON \u05DE\u05D7\u05D5\u05E5 \u05DC-Elm \u05D5\
  \u05D0\u05D6 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05DE\u05E4\u05E2\u05E0\u05D7\
  \ JSON \u05D4\u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05E9\u05DC Elm \u05DB\u05D3\u05D9\
  \ \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.236101-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD\
  \ YAML \u05D1-Elm, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05D9\u05E9 \u05E6\
  \u05D5\u05E8\u05DA \u05DC\u05D4\u05DE\u05D9\u05E8 YAML \u05DC-JSON \u05DE\u05D7\u05D5\
  \u05E5 \u05DC-Elm \u05D5\u05D0\u05D6 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\
  \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05DE\
  \u05E4\u05E2\u05E0\u05D7 JSON \u05D4\u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05E9\u05DC\
  \ Elm \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05D4\u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
כדי להתמודד עם YAML ב-Elm, בדרך כלל יש צורך להמיר YAML ל-JSON מחוץ ל-Elm ואז להשתמש בפונקציונליות המפענח JSON המובנית של Elm כדי לעבוד עם הנתונים. למרות שגישה זו דורשת שלב המרה נוסף, היא מנצלת את מערכת הטיפוסים החזקה של Elm לשם שמירה על שלמות הנתונים. כלים פופולריים להמרת YAML ל-JSON כוללים ממירים מקוונים או שירותי backend. ברגע שיש לך JSON, אתה יכול להשתמש במודול `Json.Decode` של Elm כדי לעבוד עם הנתונים.

ראשית, בהנחה שיש לך את הנתוני YAML הבאים:

```yaml
person:
  name: Jane Doe
  age: 30
```

המר אותם לפורמט JSON:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

לאחר מכן, הגדר את הדגם והמפענח שלך ב-Elm:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

כדי להשתמש במפענח זה להמיר JSON לטיפוס של Elm:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("שלום, " ++ person.name ++ "!")

        Err _ ->
            Html.text "אירעה שגיאה בעת פענוח."
```

פלט (מוצג ביישום Elm):
```
שלום, ג'יין דו!
```

גישה זו מבטיחה שתוכל לעבוד עם נתוני YAML ב-Elm על ידי שימוש ב-JSON כפורמט ביניים, תוך ניצול יכולות הדקודינג ומערכת הטיפוסים הרובסטית של Elm כדי למנוע ולשלוט בנתונים חיצוניים באופן בטוח ויעיל.
