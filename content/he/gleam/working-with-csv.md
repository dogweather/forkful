---
title:                "עבודה עם קובץ csv"
html_title:           "Gleam: עבודה עם קובץ csv"
simple_title:         "עבודה עם קובץ csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

# למה

CSV היא אחת מסוגי הקבצים הנפוצים ביותר בעולם התכנות והניתוח נתונים. משתמשים בקובץ זה כדי לאחסן נתונים בפורמט מבוזר וקל לקריאה, מה שהופך אותו לפופולרי ביותר בהעברת נתונים בין מערכות שונות. 

בפועל, עבודה עם קבצי CSV היא חלק מהיומיום של מתכנתים, מנתחי נתונים ומפתחי תוכנה. אם אתה רוצה לחקור נתונים שמאוחסנים בקובץ CSV או לתכנת פתרונות שיעזרו לך לנהל נתונים מבוזרים, העבודה עם CSV היא מומלצת ביותר.

# איך לעבוד עם CSV

כדי לעבוד עם CSV ב-Gleam, עלינו להתקין את הספרייה CSV של הקהילה ההודית. לאחר מכן, נוכל להתחיל לקרוא ולכתוב לקובץ בעזרת הפונקציות המתאימות.

לדוגמה, ננסה לקרוא את קובץ ה-CSV הבא:

```
Name,Age,Country
John,34,USA
Sara,28,Canada
Tom,42,Germany
```

לקרוא את הקובץ הזה, נצטרך להתשמש בפונקציית CSV.read. הנה דוגמה לשימוש בפונקציה זו כדי לקרוא את שם האישים מהקובץ הנתון:

```Gleam
import gleam/csv

let result = CSV.read("file.csv")
case result {
  Ok(csv) -> csv
    |> List.map(fn(entry) -> entry[0] end)
  Err(err) -> Err("Failed to read file")
}
```

הפלט של הקוד הזה יהיה רשימה עם השמות של האנשים. שנית, אם נרצה לכתוב נתונים חדשים לקובץ CSV, נוכל לעשות זאת באמצעות הפונקציה CSV.write כדלקמן:

```Gleam
import gleam/csv

let data = [["James", 32, "UK"], ["Lily", 35, "Australia"]]

result = data |> CSV.write("new_file.csv")

case result {
  Ok(_) -> IO.print("Successfully wrote to file")
  Err(err) -> IO.print("Failed to write to file")
}
```

בדוגמה הזאת, אנחנו מדפיסים הודעה בהתא