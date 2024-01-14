---
title:                "Swift: בדיקה אם תיקייה קיימת"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

ישנם מספר סיבות למה באנו לבדוק אם תיקייה קיימת בשפת סוויפט. הראשון הוא מצב בו אנו רוצים להיות בטוחים שהתיקייה שאנו מנסים ליצור כבר לא קיימת. השני הוא יכולת לבדוק אם תיקייה קיימת כדי לפעול בהתאם, למשל יצירת קובץ חדש או קריאת רשימת קבצים שנמצאים בתיקייה זו.

## איך לבדוק אם תיקייה קיימת

בשפת סוויפט, כדי לבדוק אם תיקייה קיימת אנו משתמשים בפונקציה `fileExists(atPath:)` של מחלקת `FileManager`. ניתן להזין לפונקציה את הנתיב המלא של התיקייה שאנו מעוניינים לבדוק:

```
let fileManager = FileManager.default
let path = "/Users/username/Documents/newFolder"

if fileManager.fileExists(atPath: path) {
    print("The folder exists")
} else {
    print("The folder does not exist")
}
```

לתיקיות ברמת הראשון של האפליקציה, ניתן להשתמש במערכת ההפניה `Bundle.main` כדי לקבל את נתיב התיקייה באופן ידני. לדוגמה:

```
if let path = Bundle.main.path(forResource: "folderName", ofType: nil) {
    print("The folder exists")
} else {
    print("The folder does not exist")
}
```

## שכבר חזרנו מפעילת `fileExists(atPath:)`?

ברגע שאנו מפעילים את הפונקציה `fileExists(atPath:)`, הנתיב מתייחס לשפת אופן. אם נמצא במצב שהתוכנית שלנו פותחת תיקייה בתוך תיקייה קיימת, ומעבירה לפונקציה `fileExists(atPath:)` "folder1/folder2", תת's נקבל תוצאה ששפה מתייחסת לתיקייה folder2 הנמצאת בתוך התיקייה אנו בודקים. אבל אם נמצא במצב שבו התוכנית שלנו פותחת תיקייה בתוך תיקייה קיימת, ומעבירה לפונקציה `fileExists(atPath:)` "folder1/folder2", נקבל תוצאה "The folder does not exist" כי שפה לא יכולה להתייחס לת