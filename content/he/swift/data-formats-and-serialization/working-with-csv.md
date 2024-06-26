---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:55.336344-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Swift, \u05D0\
  \u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D9\u05D9\u05D7\u05D5\u05D3\u05D9\
  \u05EA \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E7\u05D1\u05E6\u05D9 CSV \u05D9\u05E9\
  \u05D9\u05E8\u05D5\u05EA, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05EA\
  \u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9 CSV \u05D1\u05D0\
  \u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E9\u05D9\
  \u05D8\u05D5\u05EA `String` \u05DC\u05E4\u05D9\u05E6\u05D5\u05DC \u05D4\u05EA\u05D5\
  \u05DB\u05DF, \u05D0\u05D5 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05D4\u05E4\u05E2\u05DC\
  \u05EA\u2026"
lastmod: '2024-03-13T22:44:39.942767-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Swift, \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D9\u05D9\
  \u05D7\u05D5\u05D3\u05D9\u05EA \u05DC\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E7\u05D1\
  \u05E6\u05D9 CSV \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA, \u05D0\u05DA \u05E0\u05D9\
  \u05EA\u05DF \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05E0\u05EA\
  \u05D5\u05E0\u05D9 CSV \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E9\u05D9\u05DE\
  \u05D5\u05E9 \u05D1\u05E9\u05D9\u05D8\u05D5\u05EA `String` \u05DC\u05E4\u05D9\u05E6\
  \u05D5\u05DC \u05D4\u05EA\u05D5\u05DB\u05DF, \u05D0\u05D5 \u05E2\u05DC \u05D9\u05D3\
  \u05D9 \u05D4\u05E4\u05E2\u05DC\u05EA \u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\
  \u05D3 \u05D2' \u05DB\u05DE\u05D5 SwiftCSV \u05DC\u05D2\u05D9\u05E9\u05D4 \u05DE\
  \u05E1\u05D5\u05D3\u05E8\u05EA \u05D9\u05D5\u05EA\u05E8."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD CSV"
weight: 37
---

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
