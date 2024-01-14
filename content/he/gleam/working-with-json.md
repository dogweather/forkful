---
title:                "Gleam: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-json.md"
---

{{< edit_this_page >}}

עבור מה: רק 1-2 משפטים שסופרים *למה* מישהו יבחר לעבוד עם קבצי JSON.

עבור כמו לנסות: דוגמאות קוד ופלט עבור פעולות מוחלטות תוך שימוש בבלוקי קוד "```Gleam ... ```". 

Deep Dive: מידע עמוק יותר על עבודה עם קבצי JSON.

צפייה גם: רשימת קישורים. 

## עבור מה 

עבור המתכנתים החדשים לשפת גלים, עבודה עם נתוני JSON היא כלי חיוני בכדי לבנות אפליקציות ואתרים מתקדמים. קבצי JSON מאפשרים לנו לאחסן ולשלוח מידע מורכב, והם משמשים ככלי נפתח וסטנדרטי בעולם הפיתוח. יתר על כן, גלים מציעה חיבור מצוין לנתוני JSON, המאפשר לנו לעבוד בקלות עם נתונים מורכבים ולבנות אפליקציות חכמות ומתקדמות.

## כמו לנסות

הנה דוגמאות לכיצד לעבוד עם נתוני JSON בשפת גלים:

```
Gleam 
/// מיצור מבנה נתונים של JSON
type alias User = {
    name: String,
    age: Int,
    hobbies: List(String)
}

/// עבור למחלקת JSON
userDecoder: Json.Decoder User
userDecoder =
    Json.at ["name"] Json.string
        |> map2 User
        (Json.at ["age"] Json.int)
        (Json.at ["hobbies"] (list Json.string))

/// דוגמאות לסטרינגים עם נתוני JSON
output1 = """
{
    "name": "John",
    "age": 25,
    "hobbies": ["reading", "hiking", "cooking"]
}
"""

Decode.decodeString userDecoder output1 ==> Ok { name = "John", age = 25, hobbies = ["reading", "hiking", "cooking"] }

output2 = """
{
    "name": "Sarah",
    "age": 30,
    "hobbies": ["painting", "dancing"]
}
"""

Decode.decodeString userDecoder output2 ==> Ok { name = "Sarah", age = 30, hobbies = ["painting", "dancing"] }
```

## Deep Dive

עם גלים, העבודה עם נתוני JSON היא יתרון גדול. הוא מאפשר לנו לא להתמקד רק בקריאה וכתיבה של נתונים בפורמט מבני, אלא גם לבנות טיפוסים מא