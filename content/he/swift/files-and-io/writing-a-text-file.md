---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:21.196126-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D1\u05E9\u05E4\u05EA Swift \u05DE\u05D0\u05E4\u05E9\
  \u05E8\u05EA \u05DC\u05DA \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\
  \u05D9 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05D0\u05D5\u05E4\u05DF \u05E7\
  \u05D1\u05D5\u05E2 \u05E2\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\
  \u05E6\u05D9\u05DD, \u05D3\u05D1\u05E8 \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DC\u05DE\
  \u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E9\u05DE\u05D9\u05E8\u05EA\
  \ \u05D4\u05D2\u05D3\u05E8\u05D5\u05EA \u05EA\u05E6\u05D5\u05E8\u05D4, \u05E0\u05EA\
  \u05D5\u05E0\u05D9 \u05DE\u05E9\u05EA\u05DE\u05E9, \u05D0\u05D5 \u05DC\u05D5\u05D2\
  \u05D9\u05DD.\u2026"
lastmod: '2024-03-13T22:44:39.936260-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05E9\u05DC \u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05D1\u05E9\u05E4\u05EA Swift \u05DE\u05D0\u05E4\u05E9\u05E8\
  \u05EA \u05DC\u05DA \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05D0\u05D5\u05E4\u05DF \u05E7\u05D1\
  \u05D5\u05E2 \u05E2\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\
  \u05D9\u05DD, \u05D3\u05D1\u05E8 \u05D7\u05D9\u05D5\u05E0\u05D9 \u05DC\u05DE\u05E9\
  \u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E9\u05DE\u05D9\u05E8\u05EA \u05D4\
  \u05D2\u05D3\u05E8\u05D5\u05EA \u05EA\u05E6\u05D5\u05E8\u05D4, \u05E0\u05EA\u05D5\
  \u05E0\u05D9 \u05DE\u05E9\u05EA\u05DE\u05E9, \u05D0\u05D5 \u05DC\u05D5\u05D2\u05D9\
  \u05DD."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## מה ולמה?

כתיבה של קובץ טקסט בשפת Swift מאפשרת לך לשמור נתוני מחרוזת באופן קבוע על מערכת הקבצים, דבר חיוני למשימות כמו שמירת הגדרות תצורה, נתוני משתמש, או לוגים. תוכניתנים לעיתים קרובות עושים זאת כדי לשמור נתונים בין הפעלות יישום שונות, לשתף נתונים בין חלקים שונים של יישום, או לייצא נתונים לשימוש על ידי תוכניות אחרות.

## איך לעשות:

### שימוש בספריית הסטנדרט של Swift

ספריית הסטנדרט של Swift כוללת את כל הכלים הנדרשים לכתיבת קבצי טקסט. הנה גישה בסיסית:

```swift
import Foundation

let content = "שלום, קוראי Wired! ללמוד Swift זה כיף."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/example.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("הקובץ נכתב בהצלחה")
} catch let error as NSError {
    print("נכשל בכתיבה ל-URL: \(fileName), שגיאה: " + error.localizedDescription)
}
```

קטע קוד זה כותב מחרוזת לקובץ בשם `example.txt` בתיקיית המסמכים. הוא מתמודד עם שגיאות אפשריות באמצעות ניהול השגיאות do-try-catch של Swift.

### שימוש ב-FileManager לשליטה רבה יותר

לשליטה רבה יותר על מאפייני הקובץ או לבדוק אם הקובץ כבר קיים, ניתן להשתמש ב-`FileManager`:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("example.txt")
    let content = "חקירה של Swift לניהול קבצים מוארת."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("הקובץ כבר קיים")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("הקובץ נוצר ונכתב בהצלחה")
        } catch {
            print("שגיאה בכתיבת הקובץ: \(error)")
        }
    }
}
```

### שימוש בספריות צד שלישי

אחת הספריות הפופולריות עבור פעולות במערכת הקבצים ב-Swift היא `Files` מאת John Sundell:

ראשית, הוסף את Files לפרויקט שלך, בדרך כלל דרך מנהל חבילות Swift.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: ["Files"]),
    ]
)
```

לאחר מכן, השתמש בה לכתיבה לקובץ:

```swift
import Files

do {
    let file = try File(path: "/path/to/your/directory/example.txt")
    try file.write(string: "Swift וספריית Files יוצרים שילוב עוצמתי.")
    print("הקובץ נכתב בהצלחה באמצעות ספריית Files.")
} catch {
    print("אירעה שגיאה: \(error)")
}
```

עם ספריית `Files`, ההתמודדות עם קבצים הופכת לפשוטה יותר, מה שמאפשר לך להתמקד בלוגיקה העסקית של היישום שלך ולא בפרטים הטכניים של ניהול קבצים.
