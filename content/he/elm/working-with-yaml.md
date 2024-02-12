---
title:                "עבודה עם YAML"
aliases:
- he/elm/working-with-yaml.md
date:                  2024-02-03T19:25:47.831871-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
Elm אינו תומך מובנה ב-YAML, פורמט סידור נתונים שנפוץ לשימוש בקבצי תצורה או בשיתוף נתונים, בשל הדגש החזק על בטיחות טיפוס ותוצאות צפויות. עם זאת, מתכנתים לעיתים קרובות נתקלים ב-YAML בעת עבודה עם API-ים או תצורות בפיתוח אינטרנט, מה שדורש שיטות אמינות לניתוח נתוני YAML לתוך האקוסיסטם המקולקל מבחינת טיפוסים של Elm לשם אינטגרציה ושינוי חלקים.

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
