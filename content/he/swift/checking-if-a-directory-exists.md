---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:59:10.090576-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם תיקייה קיימת זו תהליך פשוט - אתה מוודא שתיקייה כבר קיימת במערכת הקבצים לפני שתנסה לקרוא ממנה, לכתוב אליה, או ליצור אותה. מתכנתים עושים את זה כדי למנוע שגיאות בעת פעולות עם קבצים.

## איך לעשות:
בנות נתיב ובדיקת קיום בSwift:

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/path/to/directory"

if fileManager.fileExists(atPath: directoryPath) {
    print("התיקייה קיימת!")
} else {
    print("התיקייה לא קיימת!")
}
```

תוצאת דוגמא:
```
התיקייה קיימת!
```
או:
```
התיקייה לא קיימת!
```

## צלילה לעומק
בSwift, `FileManager` היא הדרך לנהל קבצים ותיקיות.
היסטוריה: לפני `FileManager`, היינו משתמשים ב`NSFileManager`, שהיא חלק מObjective-C. במעבר לSwift, הAPI שופר והוא יותר "Swifty".
אלטרנטיבות: אפשר גם להשתמש בממשק של `POSIX` בפונקציה `stat` או `opendir` בSwift לבדיקות דומות.
פרטי היישום: `fileExists(atPath:)` מחזיר `true` או `false`, תלוי אם הנתיב קיים ומדובר בתיקייה. שימו לב: היא לא תבדוק אם התיקייה זמינה לקריאה/כתיבה.

## ראה גם
- התיעוד הרשמי של Apple ל `FileManager`: [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- סקירה של מערכת הקבצים על macOS: [File System Basics](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html)