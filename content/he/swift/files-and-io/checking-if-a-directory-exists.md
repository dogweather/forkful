---
aliases:
- /he/swift/checking-if-a-directory-exists/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:35.374501-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05EA\u05D9\u05E7\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA\
  \ \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD \u05D4\u05D9\u05D0 \u05D7\u05E9\u05D5\u05D1\
  \u05D4 \u05DC\u05E0\u05D9\u05D4\u05D5\u05DC \u05DE\u05D1\u05E0\u05D9 \u05E7\u05D1\
  \u05E6\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05D4\u05D0\u05E4\u05DC\u05D9\u05E7\
  \u05E6\u05D9\u05D5\u05EA \u05E9\u05DC\u05DA \u05D1-Swift. \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05D6\u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05E4\u05EA\
  \u05D7\u05D9\u05DD \u05DC\u05D0\u05DE\u05EA \u05D0\u05EA \u05E0\u05D5\u05DB\u05D7\
  \u05D5\u05EA \u05D4\u05EA\u05D9\u05E7\u05D9\u05D5\u05EA \u05DC\u05E4\u05E0\u05D9\
  \ \u05E0\u05D9\u05E1\u05D9\u05D5\u05DF\u2026"
lastmod: 2024-02-18 23:08:53.218704
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05EA\u05D9\u05E7\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1\u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\
  \u05E7\u05D1\u05E6\u05D9\u05DD \u05D4\u05D9\u05D0 \u05D7\u05E9\u05D5\u05D1\u05D4\
  \ \u05DC\u05E0\u05D9\u05D4\u05D5\u05DC \u05DE\u05D1\u05E0\u05D9 \u05E7\u05D1\u05E6\
  \u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05D4\u05D0\u05E4\u05DC\u05D9\u05E7\u05E6\
  \u05D9\u05D5\u05EA \u05E9\u05DC\u05DA \u05D1-Swift. \u05DE\u05E9\u05D9\u05DE\u05D4\
  \ \u05D6\u05D5 \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05E4\u05EA\u05D7\
  \u05D9\u05DD \u05DC\u05D0\u05DE\u05EA \u05D0\u05EA \u05E0\u05D5\u05DB\u05D7\u05D5\
  \u05EA \u05D4\u05EA\u05D9\u05E7\u05D9\u05D5\u05EA \u05DC\u05E4\u05E0\u05D9 \u05E0\
  \u05D9\u05E1\u05D9\u05D5\u05DF\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת במערכת הקבצים היא חשובה לניהול מבני קבצים מתוך האפליקציות שלך ב-Swift. משימה זו מאפשרת למפתחים לאמת את נוכחות התיקיות לפני ניסיון לקרוא מתוכן או לכתוב אליהן, ובכך למנוע שגיאות זמן ריצה אפשריות.

## איך לעשות זאת:

מסגרת ה-Foundation של Swift מספקת את המחלקה `FileManager`, שיש לה מתודות לניהול מערכת הקבצים. ניתן להשתמש ב-`FileManager` כדי לבדוק אם תיקייה קיימת. הנה קטע קוד על איך לעשות זאת:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("התיקייה קיימת")
} else {
    print("התיקייה אינה קיימת")
}
```

עם זאת, זה בודק הן קבצים והן תיקיות. אם אתה רוצה ספציפית לאמת שתיקייה קיימת, יש להעביר מצביע לערך בוליאני ב-`isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("התיקייה קיימת")
} else {
    print("התיקייה אינה קיימת")
}
```

### שימוש בספרייה חיצונית

נכון לעכשיו, בדיקה לקיום תיקייה ב-Swift לרוב לא דורשת ספריות צד שלישי בשל העמידות של מחלקת ה-`FileManager`. עם זאת, לניהול קבצים יותר מורכב ובדיקה, ספריות כמו **Files** מאת John Sundell מספקות API ידידותי יותר ל-Swift.

הנה איך תוכל להשתמש בזה:

ראשית, הוסף את Files לפרויקט שלך דרך מנהל חבילות Swift.

אז, תוכל לבדוק את קיום התיקייה כך:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("התיקייה קיימת")
} catch {
    print("התיקייה אינה קיימת")
}
```

הערה: מכיוון שספריות צד שלישי יכולות להשתנות, תמיד פנה לתיעוד העדכני ביותר לשימוש ולמתודות המומלצות ביותר.
