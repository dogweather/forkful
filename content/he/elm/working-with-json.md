---
title:                "עבודה עם json"
html_title:           "Elm: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-json.md"
---

{{< edit_this_page >}}

## למה

עבודה עם JSON נחשבת לידע חשוב כאשר מתעסקים בתכנות ומידע של שפות מפורסמות כמו Javascript. אם אתם מעוניינים להמשיך ולפתח את היכולת שלכם לעבוד עם מבני נתונים בפורמט JSON באופן יעיל וקריא, אז העבודה עם Elm תהיה מיוחדת עבורכם.

## איך לעבוד עם JSON באמצעות Elm

עבודה עם JSON באמצעות Elm היא פשוטה וידידותית. מחברים כניסה בשפת Elm תמצאו את Json.Decode משמש כקורא ומסדר את הנתונים על פני טיפוס משתנה.

```Elm
-- כל מבנה הנתונים נמצא בתוך מודול Json.Decode
-- כאשר יש לנו את הנתונים שלנו בתוך משתנה לפי הנוסחה הבאה:
myData : Decoder Int -- יצירת טיפוס
myData = Decode.int -- כאן נמצא הפורמט ששמנו אותו למשתנה
-- עכשיו נשתמש ב-myData כדי להקליד את הנתינים שלנו
Decode.decodeString myData "123" -- כעת יוצאים עם Just 123
Decode.decodeString myData "abc" -- כשימתקלים במספרים של אותיות יוצאים עם Nothing
```

## ירידה לעומק

בנוסף ליצירת טיפוסים למבני נתונים, ניתן גם להשתמש בתכניות Json.Encode כדי ליצור נתונים בפורמט JSON. זה מאפשר לנו לכתוב דומיין עם הנתונים ולשלוחם למתאם. כדי להתחיל, אנו נשתמש בברירת המחדל elm/parser, שאיננה אשרותה להתמודד בעבודה עם JSON.

```Elm
import Json.Encode as Encode
import Json.Decode as Decode

person : Encode.Value
person =
    Encode.object
        [ ( "name", Encode.string "John" )
        , ( "age", Encode.int 25 )
        , ( "married", Encode.bool True )
        ]

-- הפלט מציג נתונים של האיש "ג'ון"
Encode.encode 0 person

-- ניתן גם להשתמש ב-tags כדי ליצור טיפוס נתונים מסוג זה
type alias PersonDetails =
    { name : String
    , age : Int
    , married