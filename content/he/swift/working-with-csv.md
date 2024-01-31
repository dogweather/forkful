---
title:                "עבודה עם קבצי CSV"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"

category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם קובצי CSV מתייחסת לעיבוד ואינטראקציה עם נתונים המאורגנים בתצורת "ערכים מופרדים בפסיקים" – פורמט פופולרי לייצוא ויבוא של נתונים. תכניתנים עובדים עם קובצי CSV מכיוון שהם פשוטים לקריאה ולעיבוד גם על ידי מחשבים וגם על ידי בני אדם.

## איך לעשות:
```Swift
import Foundation

// קריאת CSV מקובץ
func readCSV(from filePath: String) -> String? {
    do {
        let contents = try String(contentsOfFile: filePath, encoding: .utf8)
        return contents
    } catch {
        print("Error reading the file.")
        return nil
    }
}

// פירוק לשורות ועמודות
func parseCSV(_ contents: String) -> [[String]] {
    let rows = contents.components(separatedBy: "\n")
    return rows.map { $0.components(separatedBy: ",") }
}

// דוגמא לשימוש
if let filePath = Bundle.main.path(forResource: "example", ofType: "csv"),
   let csvContent = readCSV(from: filePath) {
    let parsedData = parseCSV(csvContent)
    print(parsedData)
}

// פלט דוגמא
// [["שם", "תז", "אימייל"], ["דני", "123", "danny@example.com"], ...]
```

## עיון מעמיק
CSV, שהוחדר בשנות ה-70, הוא פורמט פשוט לייצג טבלאות נתונים כטקסט רגיל. ישנם פורמטים אלטרנטיביים כגון JSON ו-XML אשר מאפשרים מורכבות רבה יותר אך גם דורשים עיבוד מורכב יותר. ב-Swift, קריאה וניתוח של קבצי CSV מתבצעת לרוב באמצעות המרת הקובץ למחרוזת ולאחר מכן שימוש במתודות פירוק המחרוזת לשורות ועמודות.

## ראו גם
- תיעוד Swift רשמי: [https://swift.org/documentation/](https://swift.org/documentation/)
- מדריך לקריאה וכתיבה של קבצי CSV ב-Swift: [https://www.hackingwithswift.com/example-code/system/how-to-parse-a-csv-file-into-an-array](https://www.hackingwithswift.com/example-code/system/how-to-parse-a-csv-file-into-an-array)
- דיון בפורומים על ניתוח נתוני CSV ב-Swift: [https://forums.swift.org/](https://forums.swift.org/)
