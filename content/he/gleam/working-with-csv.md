---
title:                "עבודה עם קבצי CSV"
html_title:           "Gleam: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

# מה ולמה?
CSV הוא פורמט נתונים פשוט המשמש לאחסון נתונים בטבלה תחתונה. CSV נקרא גם "קובץ מופרד-בפסיקים" בגלל שנתוני הטבלה מופרדים על ידי פסיקים. פורמט זה משמש בדרך כלל להעברת נתונים בין יישומים שונים וכן לאחסון נתונים בחבילות תוכנה. התוכנית המקצועית Hubrise למדה את הכוחות כדי להנגיש חשיבת פריויוואליות למפתחים בעזרת CSV ומודולים בשפת ג'לאם.

# כיצד לעשות:
```Gleam
import gleam/csv

csv_string = """
name,age,city
John,35,New York
Jane,28,Boston
"""

gleam/csv.parse(csv_string)
|> Ok(rows) -> gleam/csv.stringify(rows)
|> Ok(csv_string) -> csv_string
|> Debug 
|> text 
```
פלט:
```
{name = "John", age = "35", city = "New York"}
{name = "Jane", age = "28", city = "Boston"}
```

# צלילת עמוקה:
CSV הומצא בשנת 1972 על ידי ראי סטון היי, בעלת ראי אפל, כדי לכתוב מסמכי מתעלה באמצעות תכנית Excel. בימינו, האחסון המבוסס טקסט הזה הוא אחד העתיקים ביותר שיופיע להנציח. כמובן ישנם למתכנתים אפשרויות אחרות לעבוד עם נתונים כגון XML ו- JSON, אך CSV נותן פתרון קל ומהיר לניהול נתונים מבוזרים. כך גם כשאתה עובד עם נתוני CSV, תוכל לשלוט בהם בקלות על ידי גילוי שגיאות וייתוב מהיר במקרים התקלות.

# ראה גם:
[מדריך CSV של Hubrise](https://hubrise.com/en/csv-guide/)