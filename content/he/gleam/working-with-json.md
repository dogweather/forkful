---
title:                "עבודה עם json"
html_title:           "Gleam: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON היא פעולה נפוצה בתכנות, שבה נפעל על נתונים בפורמט טקסטואלי פשוט וקריא. תוכניות וסקריפטים נעיצים לא מתאימים לעבודה עם מידע מסוג זה, ולכן תוכניות המטרה שלהן הינן על נתוני JSON נעיצים לא תואמים שהם נמשכים ממקורות רבים ושונים.

## איך לבצע:
איות וכאלה לפני הסקריפט שלנו תוכנית דמה. הנה כמה דוגמאות עם דגש על הפלט מצוטט כדווח Gleam פקודת:

```Gleam
import gleam/json

let json = """
{
    "name": "John",
    "age": 30,
    "hobbies": ["hiking", "reading"],
    "address": {
        "city": "Tel Aviv",
        "country": "Israel"
    }
}
"""

let parsed_json = json
    |> json.parse
expect(parsed_json) |> to_equal(Ok({
    "name" => "John",
    "age" => 30,
    "hobbies" => ["hiking", "reading"],
    "address" => {
        "city" => "Tel Aviv",
        "country" => "Israel"
    }
}))

let encoded_json = parsed_json
    |> json.encode
expect(encoded_json) |> to_equal(Ok(json))
```

## Deep Dive:
על CSV ואלטרנטיבות JSON נמשכים מטורים, כמו גם היסטוריה מוצגת יותר מורכבות עסקיות. פתוח JSON בנוחות! מיש הרבה נתונים של נתונים משתמשים לדעת הנתונים. תוכניות ומערכות המספקות כולם CSV בגרושים ושנעת מחולון בנות כפלט בנוחו JSON.

## ראו גם:
למידע נוסף על Gleam ועבודה עם JSON, מומלץ להסתכל על המקורות הבאים:

- מדריך של Gleam לעבודה עם קבצי JSON: https://gleam.run/book/core-modules-json.html
- תיעוד רשמי של המודול gleam/json: https://hexdocs.pm/gleam_json/