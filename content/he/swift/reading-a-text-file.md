---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט היא פעולה בה מפענח התוכנית מידע שמשמש בתוכניות שונות. מתכנתים קוראים קבצים מטקסט כדי לנתח נתונים, לנהל מידע, או כאשר יש צורת לקוח חיצונית עם דרישות מסוימות.

## איך לעשות:
נחפש באמצעות Swift קובץ טקסט בשם `example.txt`.
קאוד:
```swift
import Foundation

let fileURL = Bundle.main.url(forResource: "example", withExtension: "txt")
do {
    let content = try String(contentsOf: fileURL!, encoding: .utf8)
    print(content)
} catch {
    print("Failed reading from URL: \(fileURL), Error: " + error.localizedDescription)
}
```
פלט:
```
Hello, Swift!
```
כאן, אנו משתמשים ב- `String(contentsOf:, encoding:)` כדי לקרוא את הקובץ.

## צלילה עמוקה
היסטורית, Swift נבנה מאט במשך שנים כדי להיות שפה שתהיה גמישה, מהירה וידידותית למשתמש. הקריאה של קובץ טקסט היא אחת מן התכונות שמאפשרות גמישות זו.
בנוסף, ישנם שיטות חלופיות לקריאת קבצי טקסט, כמו פעולות נמוכות יותר של אלגוריתמים כמו fread ב-C, אך Swift מספקת גישה פשוטה יותר ומבנית למטרה זו.

## ראו גם
- [Reading and Writing Files in Swift](https://www.hackingwithswift.com/read/0/17/reading-and-writing-files-with-string)
- [Swift Documentation: String(contentsOf:encoding:)](https://developer.apple.com/documentation/swift/string/1414957-init)

כאן, אתם מוזמנים להכיר יותר על Swift וקריאת קבצי טקסט.