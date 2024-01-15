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

## למה

JSON היא שפת תכנות פופולרית לעבודה עם נתונים מבוזרים ומשמשת כיום במגוון יישומים וטכנולוגיות. בכתבה זו נלמד כיצד לעבוד עם JSON בשיטה פשוטה ויעילה בעזרת Gleam.

## איך לעשות זאת

נתחיל עם התקנת החבילה json בעזרת פקודת ההתקנה "brew install json" עבור משתמשי MacOS. למשתמשי Linux, ניתן להתקין בעזרת הפקודה "apt-get install json". כעת ניצור קובץ נתונים JSON פשוט עם השדות "name", "age" ו-"city" ונשמור אותו תחת שם "data.json". נתונים נרשמים כך: {"name": "דניאל", "age": 32, "city": "תל אביב"}

כעת נעשה קודד וקריאת התוכן של קובץ JSON בעזרת Gleam:

```
Gleam =>

import Gleam.Json

fn main() {
  // קוראים את תוכן הקובץ ומקבלים את התוצאה כמחרוזת
  let json_data = std.fs.read_text("data.json")

  // ממירים את המחרוזת למבנה נתונים חדש עם סוגות מוגדרים מראש
  let json_result = Gleam.Json.to_result(json_data)

  // מסדרים את הסוגים לאופן הרצוי ומודפסים אותם
  let user_result =(
    Gleam.Json.get_field(Ok("name"), json_result),
    Gleam.Json.get_field(Err("age"), json_result),
    Gleam.Json.get_field(Ok("city"), json_result)
  )

  // הוצאת תוצאת JSON מאתדראם במשתנה JSON מאת.גמא כעת באסימון המרושש של גורל
  ("דניאל", 32, "תל אביב")
}
```
נריץ את הקוד וניערך את התוצאה לתוך מערכת הגמא, כתובת הנתונים יהיה: {"דניאל", "32", "תל אביב"}

## חפירה עמוקה

עבודה עם JSON בעזרת Gleam יכולה להיות נגישה ונוחה. פעולות כמו "פילטר", "מיון", "עדכון" ו-"מחיקה" ניתנות להחיל על נתונים JSON בקלות עם השימוש בפונקציות של Gleam.Json.