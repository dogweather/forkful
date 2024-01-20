---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? / מה ולמה?
עבודה עם קבצי CSV פירושה קריאה וכתיבה של נתונים בפורמט פופולרי המכיל נתונים המופרדים בפסיקים. תוכניתנים עושים זאת כי זה דרך נוחה להתחלף בנתונים עם יישומים אחרים ולעבוד עם נתונים טבלאיים.

## How to: / איך לעשות:
```gleam
import gleam/csv

fn main() {
  let data = "שם,גיל,עיר\nדני,30,תל אביב\nרונית,25,ירושלים"
  let rows = csv.decode(data)
  case rows {
    Ok(records) -> {
      for record in records {
        // פה ניתן להשתמש בנתונים המפורסמים
        io.println(record)
      }
    }
    Error(_) -> io.println("ערה בקריאת קובץ CSV")
  }
}
```
הפלט יהיה:
```
["שם", "גיל", "עיר"]
["דני", "30", "תל אביב"]
["רונית", "25", "ירושלים"]
```

## Deep Dive / צלילה עמוקה:
פורמט CSV נוצר בשנות ה-70 ונותר פופולרי בזכות פשטותו. אלטרנטיבות כוללות JSON או XML, אבל CSV עדיין נמצא בשימוש רחב בתעשייה. בזמן עבודה עם CSV ב-Gleam, חשוב לטפל בשגיאות כמו פורמט לא תקין ולוודא כי היציאה תהיה בתצורה ידידותית.

## See Also / גם זה:
- [מדריך רשמי ל-Gleam](https://gleam.run/book)
- [מפרט הפורמט CSV](https://tools.ietf.org/html/rfc4180)