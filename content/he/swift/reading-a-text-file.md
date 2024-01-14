---
title:                "Swift: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

מה הסיבה לקרוא קובץ טקסט? ניתן לקרוא קבצי טקסט כדי לקבל מידע מכתוביות בסרטים, לייצא נתונים לפורמטים שונים ועוד.

## כיצד לעשות זאת

```swift
// פתיחת קובץ טקסט
let fileURL = URL(fileURLWithPath: "file.txt")

// קריאה של תוכן קובץ טקסט
do {
    let text = try String(contentsOf: fileURL, encoding: .utf8)
    print(text) // פלט הטקסט מהקובץ
} catch {
    print("שגיאה בקריאת הקובץ") 
}

// כתיבת נתונים לקובץ טקסט
let text = "זהו טקסט לכתיבה לקובץ"
do {
    try text.write(to: fileURL, atomically: false, encoding: .utf8)
} catch {
    print("שגיאה בכתיבת הטקסט לקובץ")
}
```

## מעמקים

כדי לקרוא קובץ טקסט, ניתן להשתמש בפונקציות של המחלקה המובנית `String` וניתן לייצא נתונים לפורמטים שונים באמצעות המחלקה `Data`. ישנן גם ספריות נוספות כמו `FileKit` שעוזרות בקריאת וכתיבת קבצי טקסט בצורה נוחה יותר.

## ראה גם

- [מדריך על כתיבה וקריאת קבצי טקסט עם Swift](https://medium.com/flawless-app-stories/read-and-write-files-in-swift-a-users-tutorial-8ff0bcae4ed5)
- [מסמך רשמי על המחלקה String ופונקציות קריאה וכתיבה](https://developer.apple.com/documentation/foundation/string)
- [ספריית FileKit לקריאה וכתיבת קבצים בסגנון יותר אינטואיטיבי](https://github.com/nvzqz/FileKit)