---
title:                "עובדים עם CSV"
aliases: - /he/swift/working-with-csv.md
date:                  2024-02-03T19:21:55.336344-07:00
model:                 gpt-4-0125-preview
simple_title:         "עובדים עם CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם קבצי CSV (ערכים מופרדים בפסיק) כוללת ניתוח ויצירת נתונים מובנים מקבצי טקסט שבהם כל שורה מייצגת רשומה וכל רשומה מורכבת משדות המופרדים בפסיקים. מתכנתים לעיתים קרובות מתעסקים בפעילות זו כדי לייבא, לייצא ולשנות נתונים טבלאיים בקלות באמצעות פורמט שנתמך ברחבי פלטפורמות ושפות תכנות שונות, בשל פשטותו ופורמט הקריא לאדם.

## איך לעשות:

ב-Swift, אין תמיכה ייחודית לניתוח קבצי CSV ישירות, אך ניתן להתמודד עם נתוני CSV באמצעות שימוש בשיטות `String` לפיצול התוכן, או על ידי הפעלת ספריות צד ג' כמו SwiftCSV לגישה מסודרת יותר. הנה שתי השיטות:

### ניתוח ידני ללא ספריות חיצוניות
```swift
// שקול מחרוזת CSV פשוטה
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// פצל את מחרוזת ה-CSV לשורות
let rows = csvString.components(separatedBy: "\n")

// חלץ את המפתחות מהשורה הראשונה
let keys = rows.first?.components(separatedBy: ",")

// עבור על השורות החל מהשנייה
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// פלט לדוגמה
print(result)
// פלט: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
גישה זו פשוטה אך חסרה עמידות, במיוחד עם קבצי CSV המכילים מקרים מיוחדים כמו פסיקים בערכים, שבירות שורה בתוך שדות וכו'.

### שימוש בספריית SwiftCSV
ראשית, הוסף את SwiftCSV לפרויקט שלך על ידי כלולה בתלות `Package.swift` שלך:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
לאחר מכן, יבא והשתמש בה כך:
```swift
import SwiftCSV

// הנח ש-`csvString` מוגדר כמו לעיל

// צור אובייקט CSV
if let csv = try? CSV(string: csvString) {
    // גש לשורות כמילונות
    let rows = csv.namedRows
    
    // פלט לדוגמה
    print(rows)
    // פלט: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV מפשטת את הניתוח על ידי התמודדות אוטומטית עם דקויות כמו פסיקים מוכלים, שבירות שורה בשדות, וקידוד תווים. עם זאת, זכור להתמודד עם שגיאות אפשריות ביישומים בעולם האמיתי, במיוחד כאשר מתמודדים עם מקורות נתונים חיצוניים.
