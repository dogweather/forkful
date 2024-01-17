---
title:                "עבודה עם קבצי CSV"
html_title:           "Go: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-csv.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
עבודה עם CSV היא תהליך שבו מחברים עם קבצי נתונים הנתמכים על ידי רבים ומוכרים. זהו דרך נוחה ויעילה לנתח ולהתאים נתונים ממקורות שונים. תהליך זה הופך חייב הדיונים לפשוט יותר ושולי את טעויות בטיפול בנתונים.

## כיצד לעשות:
לדוגמה, נוכל להשתמש ב-Package "encoding/csv" של Go כדי לבצע מניפולציה עם קבצי CSV. להלן דוגמה של קוד ופלט:
```Go
package main

import (
    "encoding/csv"
    "fmt"
    "log"
    "os"
)

func main() {
    // פתיחת קובץ קיים לקריאה
    f, err := os.Open("file.csv")
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()

    // קריאת נתונים מהקובץ בעזרת הפונקציה ReadAll של ה-Package "csv"
    records, err := csv.NewReader(f).ReadAll()
    if err != nil {
        log.Fatal(err)
    }

    // הדפסת כל רשומה הכוללת שורה של ערכים מופרדת בפסיק
    for _, record := range records {
        fmt.Println(record)
    }
}
```

הפלט יהיה משהו דומה לכך:
```Go
["אופציונלי" "תוכן" "שורת" "ערכים"]
["פרפרת" "גל" "תוכן" "פרפר" "אדומה"]
["כלב" "פודל" "תוכן" "4"]
["חתול" "סיאמי" "תוכן" "מותג" "העכבר"]
```

## כניסה מעמיקה:
CSV נוצר בשנת 1972 על ידי החברה Informatics General בכדי למיין נתונים באמצעות טבלאות. עם השנים, הוא הפך לפורמט נרחב לניהול נתונים ומצא יישומים רבים בכלל המחשבה. בנוסף, ישנן אלטרנהטיבות דומות לעבודה עם CSV כגון JSON ו-XML, אך הנהיגה עם CSV דרך פאקטור מוכר יותר. הPHPPackage מספק תמיכה מעולה בטיפול בנתוני CSV, על אף שהוא הקטנת הטריות האלגנטיות שניתן למצוא בגו.

## ראה גם:
למידע נוסף על פקטור "encoding/csv" של Go ועל יישומים שלו, אני ממליץ לך לבקר בקישור הבא:
- [פקטור "encoding/csv" של Go](https://pkg.go.dev/encoding/csv)
- [CSV: רכיב משמעותי בשיווי שכר מחברת Informatics General](https://medium.com/towards-artificial-intelligence/csv-a-significant-component-of-the-payment-structure-of-the-informatics-general-company-3bd36bb00a61)