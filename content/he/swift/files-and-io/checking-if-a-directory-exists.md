---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- /he/swift/checking-if-a-directory-exists.md
date:                  2024-02-03T19:09:35.374501-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
