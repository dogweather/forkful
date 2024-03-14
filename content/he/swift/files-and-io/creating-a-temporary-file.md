---
date: 2024-01-20 17:42:00.122642-07:00
description: "\u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05E7\u05D5\u05D1\u05E5 \u05D6\
  \u05DE\u05E0\u05D9 \u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E6\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05DE\u05E7\u05D5\u05DD \u05D6\u05DE\u05E0\u05D9\
  \ \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05DE\
  \u05D4\u05DC\u05DA \u05E8\u05D9\u05E6\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA. \u05D6\u05D4 \u05E0\u05E2\u05E9\u05D4 \u05DC\u05DE\u05D8\u05E8\u05D5\u05EA\
  \ \u05E9\u05D5\u05E0\u05D5\u05EA, \u05DB\u05DE\u05D5 \u05DC\u05D3\u05D5\u05D2\u05DE\
  \u05D4 \u05E0\u05D9\u05D4\u05D5\u05DC \u05DE\u05D0\u05D2\u05E8 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05E7\u05D9\u05D1\u05D5\u05E5 \u05E4\u05E8\u05D8\u05D9\u05DD\
  \ \u05D1\u05D9\u05E0\u05DC\u05D0\u05D5\u05DE\u05D9\u05D9\u05DD,\u2026"
lastmod: '2024-03-13T22:44:39.937957-06:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05DB\u05D0\u05E9\u05E8 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E6\u05E8\
  \u05D9\u05DB\u05D9\u05DD \u05DE\u05E7\u05D5\u05DD \u05D6\u05DE\u05E0\u05D9 \u05DC\
  \u05E9\u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1\u05DE\u05D4\
  \u05DC\u05DA \u05E8\u05D9\u05E6\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  . \u05D6\u05D4 \u05E0\u05E2\u05E9\u05D4 \u05DC\u05DE\u05D8\u05E8\u05D5\u05EA \u05E9\
  \u05D5\u05E0\u05D5\u05EA, \u05DB\u05DE\u05D5 \u05DC\u05D3\u05D5\u05D2\u05DE\u05D4\
  \ \u05E0\u05D9\u05D4\u05D5\u05DC \u05DE\u05D0\u05D2\u05E8 \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05E7\u05D9\u05D1\u05D5\u05E5 \u05E4\u05E8\u05D8\u05D9\u05DD \u05D1\
  \u05D9\u05E0\u05DC\u05D0\u05D5\u05DE\u05D9\u05D9\u05DD,\u2026"
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
---

{{< edit_this_page >}}

## מה ולמה?

יוצרים קובץ זמני כאשר אנחנו צריכים מקום זמני לשמור נתונים במהלך ריצת התוכנית. זה נעשה למטרות שונות, כמו לדוגמה ניהול מאגר נתונים, קיבוץ פרטים בינלאומיים, או לשמירת המצב באפליקצייה.

## איך לעשות:

בSwift, אנו יכולים ליצור קבצים זמניים בעזרת מנהל הקבצים FileManager. הנה כיצד לעשות זאת:

```Swift
import Foundation

func createTemporaryFile() -> URL? {
    let temporaryDirectoryURL = FileManager.default.temporaryDirectory
    let temporaryFilename = ProcessInfo.processInfo.globallyUniqueString
    let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent(temporaryFilename)
    
    do {
        try "Temporary data".write(to: temporaryFileURL, atomically: true, encoding: .utf8)
        print("Temporary file created at \(temporaryFileURL)")
        return temporaryFileURL
    } catch {
        print("Error creating temporary file: \(error)")
        return nil
    }
}

if let temporaryFileURL = createTemporaryFile() {
    // השתמש/י בקובץ כאן. לאחר מכן, ניתן למחוק אותו.
}
```

בדוגמא לעיל, יצרנו פונקציה שמייצרת קובץ זמני וכותבת נתונים אליו. היא מחזירה את כתובת הURL של הקובץ או nil במקרה של שגיאה.

## ניתוח מעמיק:

ליצירת קובץ זמני יש היסטורייה ארוכה בתכנות ומערכות הפעלה. היא מאפשרת בדיקות, הפעלות, וטיפול בנתונים הדורשים חייוורון לאחר שימוש. בנוסף, קבצים זמניים מסייעים במניעת זיהום ההארד דיסק עם נתונים לא רצויים. בSwift, ניצולו של FileManager הוא הדרך הסטנדרטית לנהל קבצים.

עם זאת יש גם אלטרנטיבות. במערכות יוניקס, ניתן לנצל את מערכת הקבצים /tmp שמיועדת לקבצים זמניים. בSwift ישנה גם האפשרות לבחירת הדרך בה נוצר הקובץ על ידי השגת בית מערכת אחר לאחסון (לדוגמא, באמצעות המתודה url(for:in:appropriateFor:create:)).

## ראה גם:

- [אתר רשמי של Swift](https://swift.org/documentation/)
- [מדריך לFileManager בSwift](https://developer.apple.com/documentation/foundation/filemanager)
- [מדריך Apple לעבודה עם קבצים ונתונים](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
