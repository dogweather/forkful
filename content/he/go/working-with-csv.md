---
title:                "Go: עבודה עם קבצי csv"
simple_title:         "עבודה עם קבצי csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-csv.md"
---

{{< edit_this_page >}}

# למה

CSV היא פורמט נתונים נפוץ מאוד ונמצא בשימוש ברבים מישורי התעשייה. בעזרת שפת התכנות Go, ניתן לעבוד באופן יעיל עם קבצי CSV באמצעות מספר ספריות ופתרונות פתיחה שונים.

# כיצד לעבוד עם CSV ב-Go

כדי לקרוא קובץ CSV ב-Go, נצטרך ליצור מצב קורא ולשלוח את הנתונים לספריית CSV הפופולרית. כאן תהיה כמה דוגמאות לשימוש במספר ספריות ולטעון את הנתונים למערך של מבנה נתונים struct ב-Go.

```Go
package main
 
import (
    "encoding/csv"
    "fmt"
    "os"
)
 
type Person struct {
    Name string `csv:"name"`
    Age int `csv:"age"`
    City string `csv:"city"`
}
 
func main() {
    f, _ := os.Open("people.csv")
 
    reader := csv.NewReader(f)
    reader.FieldsPerRecord = 3
 
    records, _ := reader.ReadAll()
 
    var people []Person
 
    for _, r := range records {
        person := Person{
            Name: r[0],
            Age: r[1],
            City: r[2],
        }
        people = append(people, person)
    }
 
    fmt.Println(people)
}
```

תוכלו לראות בפלט שכל הנתונים מטענים למערך של מבנה נתונים struct, המוגדר בפורמט של CSV כדי לקלוט אותם בצורה נוחה ומובנית.

# חקירה עמוקה

למידה על שימוש נכון בספריה המתאימה לכל מטרה יכולה להיות מאתגרת בפעמים, אך בהתמדת ועם נסיון, התכנים שלקחנו מהתגובה למטה יהפכו לרגע לבטחת מפעלי האתגרים הכי מורכבים ביותר. כמעט כל תכנית Go שאתה כותב מכילה נתונים ובדרך כלל נתונים אלו אמורים לקבוע את התוכן שאתה כותב. CSV מציג שיטה פשוטה ויעילה לדאוג לכל סוג של נתונים שתוכניות Go שלך יכול לעבוד את.

# ראו גם

- [ספריית CSV רשמית ב-Go](https://golang.org/pkg/encoding/csv/)
-