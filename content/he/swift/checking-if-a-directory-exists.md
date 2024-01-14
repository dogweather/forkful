---
title:    "Swift: בדיקת קיום תיקייה"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## למה

אם אתם מתכנתים ב-Swift או חוקרים את השפה הזו, כנראה תיתקלו במצב בו תצטרכו לבדוק אם תיקייה קיימת. החשיבה הראשונה שעולה על הראש היא לדעת אם התיקייה קיימת כדי להוסיף או לעדכן קבצים בתוכה. כמו כן, ייתכן שתיקייות מסוימות נדרשות לתפקוד מסוים של האפליקציה שלכם, וכדי לוודא שהן קיימות תצטרכו לבדוק את קיומן.

## איך לעשות זאת

כדי לבדוק אם תיקייה קיימת באמצעות קוד Swift, ניתן להשתמש בפונקציה `fileExists(atPath:)` של המחלקה `FileManager`. התחביר הוא פשוט וקל לשימוש:

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Documents"

// בדיקה אם התיקייה קיימת
if fileManager.fileExists(atPath: path) {
    print("התיקייה מסומנת כ-YES")
} else {
    print("התיקייה מסומנת כ-NO")
}
```

כדי לבדוק אם תיקייה נמצאת בתוך תיקיית אב, ניתן להוסיף את שם התיקייה המבוקשת לאחר הנתיב:

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Documents/FolderName"

// בדיקה אם התיקייה נמצאת בתוך תיקיית המסמך
if fileManager.fileExists(atPath: path) {
    print("התיקייה מסומנת כ-YES")
} else {
    print("התיקייה מסומנת כ-NO")
}
```

אם התיקייה מכילה קבצי טקסט או תמונות ואתם רוצים להוסיף אותם לתיקייה הזו, ניתן לעבור על כל הקבצים ולנסות להוסיף אותם באמצעות הפונקציה `contentsOfDirectory(atPath:)`:

```Swift
let fileManager = FileManager.default
let path = "/Users/username/Documents"

// בדיקה אם יש קבצים נמצאים בתוך התיקייה והוספתם לתיקייה המבוקשת
if let files = fileManager.enumerator(atPath: path) {
    for file in files {
        if let filePath = file as? String {
            // הוספת הק