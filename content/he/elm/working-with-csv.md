---
title:                "Elm: עבודה עם קבצי csv"
simple_title:         "עבודה עם קבצי csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

למה לעסוק בכתיבת קוד עם CSV? קבצי CSV הם דרך נפלאה לאחסון נתונים טבלאיים בפורמט פשוט וקריא. המרת הנתונים ל-CSV והחזרתם ממנה היא חלק חשוב ביישומי ניתוח נתונים ותכנות סטטיסטיים, ובכתיבת קוד בכלל. באמצעות כתיבת קוד עם אלם, המפתחים מושכים עבור הקלות שחברות נתונים מאגרי נתונים כמו SQL ורירית פילד, והשתמשו בדטה פריים של האלם להגנה מבעיות של מבני נתונים אלה הניתנות לשינוי בזמן.

## איך לעשות

תחילה, נדרש להתקין את חבילת CSV של אלם באמצעות פקד log וקומנדת התקנה. לאחר מכן, ניתן ליצור מבנה לנתונים ולהמיר אותו לפורמט CSV על ידי שימוש בפונקציה encode.

```Elm
import Csv.Encode as Encode

-- מבנה לנתונים
type alias Person =
  { name : String
  , age : Int
  , city : String
  }

-- רשימת אנשים
people : List Person
people =
  [ { name = "דניאל", age = 32, city = "תל אביב" }
  , { name = "נעמה", age = 25, city = "ירושלים" }
  , { name = "משה", age = 42, city = "חיפה" }
  ]

-- ממיר מבנה לנתונים לפורמט CSV
peopleCsv : Encode.Value
peopleCsv =
  Encode.list
    [ Encode.object
        [ ("Name", Encode.string << .name)
        , ("Age", Encode.int << .age)
        , ("City", Encode.string << .city)
        ]
    ]
    people

-- פלט מרומז של המבונה
"[\"Name\", \"Age\", \"City\"],[\"דניאל\", 32, \"תל אביב\"], [\"נעמה\", 25, \"ירושלים\"], [\"משה\", 42, \"חיפה\"]"
```

כעת, ניתן להשתמש בפונקציות של חבילת CSV כדי לקרוא קובץ CSV ולהמיר אותו למבנה נתונים מתאים.

```Elm
import Csv.Decode as Decode

-- חישוב גיל ממוצע עבור קובץ CSV של אנשים
averageAge : Result String Float
averageAge =
  Decode.at [0] Decode.string -- הפעולה ת