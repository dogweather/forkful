---
aliases:
- /he/go/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:54.176467-07:00
description: "\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\
  \u05D5\u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD\
  \ (CSV) \u05D4\u05D5\u05D0 \u05E0\u05E4\u05D5\u05E5 \u05DC\u05D4\u05D7\u05DC\u05E4\
  \u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E9\u05DC \u05E4\u05E9\u05D8\
  \u05D5\u05EA\u05D5 \u05D5\u05E7\u05DC\u05D5\u05EA \u05D4\u05E9\u05D9\u05DC\u05D5\
  \u05D1 \u05E9\u05DC\u05D5 \u05E2\u05DD \u05E8\u05D5\u05D1 \u05E9\u05E4\u05D5\u05EA\
  \ \u05D4\u05EA\u05DB\u05E0\u05D5\u05EA, \u05DB\u05D5\u05DC\u05DC Go. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\
  \u05D5\u05D1\u05D5\u05EA \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD \u05E7\
  \u05D1\u05E6\u05D9 CSV\u2026"
lastmod: 2024-02-18 23:08:52.366036
model: gpt-4-0125-preview
summary: "\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05D5\
  \u05E4\u05E8\u05D3\u05D9\u05DD \u05D1\u05E4\u05E1\u05D9\u05E7\u05D9\u05DD (CSV)\
  \ \u05D4\u05D5\u05D0 \u05E0\u05E4\u05D5\u05E5 \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05E9\u05DC \u05E4\u05E9\u05D8\u05D5\
  \u05EA\u05D5 \u05D5\u05E7\u05DC\u05D5\u05EA \u05D4\u05E9\u05D9\u05DC\u05D5\u05D1\
  \ \u05E9\u05DC\u05D5 \u05E2\u05DD \u05E8\u05D5\u05D1 \u05E9\u05E4\u05D5\u05EA \u05D4\
  \u05EA\u05DB\u05E0\u05D5\u05EA, \u05DB\u05D5\u05DC\u05DC Go. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\
  \u05D5\u05EA \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD \u05E7\u05D1\u05E6\
  \u05D9 CSV\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05E7\u05D5\u05D1\u05E6\u05D9\
  \ CSV"
---

{{< edit_this_page >}}

## מה ולמה?

פורמט ערכים מופרדים בפסיקים (CSV) הוא נפוץ להחלפת נתונים בשל פשטותו וקלות השילוב שלו עם רוב שפות התכנות, כולל Go. מתכנתים לעיתים קרובות עובדים עם קבצי CSV לצורך הגירת נתונים, יצירת דוחות, או ניתוח נתונים, מה שהופך את ההבנה של מניפולציה ב-CSV לקריטית בערכת כלים לפיתוח תוכנה.

## איך לעשות:

עבודה עם קבצי CSV ב-Go היא ישירה, הודות לספריית התקן שלה, `encoding/csv`. להלן מבוא לקריאה וכתיבה של קבצי CSV.

### קריאת קובץ CSV

כדי לקרוא מקובץ CSV, תחילה פתח את הקובץ באמצעות `os.Open`, לאחר מכן צור קורא CSV חדש עם `csv.NewReader`.

```go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

קטע הקוד הזה יקרא את כל הרשומות מ-`data.csv` וידפיס אותן. כל רשומה היא מערך של שדות.

### כתיבה לקובץ CSV

לכתיבה, אתה משתמש ב-`csv.NewWriter` ו-`writer.WriteAll` או `writer.Write` לכתיבה של מרובות או רשומה יחידה של רשומות CSV, בהתאמה.

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Name", "Age", "City"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

זה יצור קובץ בשם `output.csv` עם הרשומות שסופקו. תמיד זכור לנקז את הכותב כדי לוודא שכל הנתונים בזיכרון המטמון נכתבו לקובץ.

## היכרות מעמיקה

חבילת `encoding/csv` של Go מספקת תמיכה חזקה לקריאה וכתיבה של קבצי CSV אך היא מעוצבת עם פשטות בראש, מה שאומר שהיא לא מטפלת בתרחישים מורכבים יותר כמו זיהוי אוטומטי של מפרידים, התמודדות עם מרכאות או הפרדת שורות מוטמעות בשדות ללא טיפול ידני.

באופן היסטורי, טיפול ב-CSV בשפות תכנות לעיתים קרובות היה מסורבל בשל אותן מורכבויות, אך ספריית התקן של Go מפשטת רבות מהבעיות האלה, מה שמאפשר למפתחים לעבוד עם נתוני CSV ביחסית קלות. עם זאת, לניפוי CSV מורכב יותר, ייתכן שיהיה צורך בספריות צד שלישי כמו `gocsv` או בטיפול ידני בפרסור.

אחד היבט בולט בחבילת `csv` של Go הוא התמיכה שלה בציון פסיק מותאם אישית (מפריד), מה שמאפשר לה לעבוד בצורה חלקה עם וריאנטים של קבצי CSV, כמו ערכים מופרדים בטאבים (TSV). עם זאת, כאשר מתמודדים עם קבצי CSV פחות סדירים או לא תקניים, מתכנתי Go עשויים למצוא עצמם נזקקים להרחיב את היישומים הקיימים של קורא או כותב csv.

למרות שיכולות הטיפול ב-CSV של Go חזקות למטרות כלליות, ליישומים הדורשים מניפולציה אינטנסיבית של נתונים, כמו מדעי הנתונים או משימות המרת נתונים מורכבות, מתכנתים עשויים לעיין בחבילות עיבוד נתונים מוקדשות או אפילו בשפות אחרות המתאימות יותר למשימות אלו, כמו Python עם ספריית `pandas`. עם זאת, לפעולות קריאה-כתיבה פשוטות של CSV, ספריית התקן של Go בולטת ביעילותה ופשטותה.
