---
title:                "Swift: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
ישנם מספר סיבות שעשויות לגרום למתכנתים לבדוק אם תיקייה קיימת במיקום מסוים. ייתכן שהם רוצים לוודא שהתיקייה קיימת לפני שהם מנסים לעבוד עם הקבצים שבתוכה או שהם מעוניינים ליצור את התיקייה במידה והיא לא קיימת.

## איך לבדוק אם תיקייה קיימת
בשפת Swift ישנם כמה אפשרויות לבדוק אם תיקייה קיימת:

```Swift
// בדיקה האם התיקייה קיימת במיקום מסוים
let directoryPath = "/Users/username/Documents"
let fileManager = FileManager.default

if fileManager.fileExists(atPath: directoryPath) {
    print("התיקייה קיימת")
} else {
    print("התיקייה לא קיימת")
}

```

```Swift
// יצירת התיקייה במידה והיא לא קיימת
let directoryPath = "/Users/username/Documents/new_folder"
let fileManager = FileManager.default

if !fileManager.fileExists(atPath: directoryPath) {
    do {
        try fileManager.createDirectory(atPath: directoryPath, withIntermediateDirectories: true, attributes: nil)
        print("התיקייה נוצרה בהצלחה")
    } catch {
        print("אירעה שגיאה ביצירת התיקייה")
    }
}

```

## Deep Dive
בשפת Swift, ישנם שיטות שונות לבדיקה האם תיקייה קיימת. כמו בדוגמאות שהוצגו למעלה, ניתן להשתמש בפעולת `fileExists(atPath:)` כדי לבדוק אם התיקייה קיימת. בנוסף, ניתן להשתמש בפעולות נוספות כגון `fileExists(atURL:)` ו- `contentsOfDirectory(atPath:)`.

כדי לבדוק אם התיקייה לא רק קיימת אלא גם נמצאת בכתובת מסוימת, יש להשתמש בפעולת `fileExists(atPath:)` ולפסול רק את התנאי שנמצא בתת המרחק.

## ראה גם
* [מדריך רשמי של Apple על ניהול קבצים ותיקיות בשפת Swift](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html#//apple_ref/doc/uid/TP40010672-CH1-SW1)
* [מדריך על השתמשות בפעולות ניהול קבצים בשפת Swift](https://www.hackingwithswift.com/read/0/