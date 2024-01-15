---
title:                "קריאת קובץ טקסט"
html_title:           "Swift: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קריאת קובץ טקסט היא כלי חשוב בפיתוח תוכניות בשפת Swift. באמצעות קריאת קובץ טקסט, ניתן לטעון מידע מקובץ חיצוני ולעבד אותו בתוכנית שלנו. להלן נדבר על המבנה היסודי של קבצי טקסט וכיצד לקרוא קובץ טקסט בשפת Swift.

## כיצד לקרוא קובץ טקסט

כדי לקרוא קובץ טקסט בשפת Swift, נצטרך להשתמש במחלקת `FileManager` ובפונקציות המסופקות על ידה. נתחיל עם יצירת מפענח לקובץ הטקסט על ידי השתמשות במתודת `url(forResource:withExtension:)` של `FileManager`. לדוגמה, אם קובץ הטקסט שלנו נמצא בתיקייה בשם "files" ונקרא "text_file.txt", הקוד יראה כך:

```Swift
let fileManager = FileManager.default
if let fileUrl = fileManager.url(forResource: "text_file", withExtension: "txt", subdirectory: "files") {
    // do something with the fileUrl
}
```

כעת, ניתן לקרוא את תוכן הקובץ על ידי השתמשות במתודת `contents` של `String` והבאת התוכן למשתנה מסוג `String`. לדוגמה:

```Swift
let fileManager = FileManager.default
if let fileUrl = fileManager.url(forResource: "text_file", withExtension: "txt", subdirectory: "files"),
    let text = try? String(contentsOf: fileUrl) {
        print(text)
}
```

כעת, המשתנה `text` מכיל את כל הטקסט שנמצא בקובץ הטקסט ונוכל לבצע עליו פעולות נוספות כפי שנדרש לתוכנית שלנו.

## חקירה מעמיקה

בתוך קובץ טקסט, המידע מאוחסן בסדר לוגי כדי שהמחשב יוכל לקרוא אותו נכון. המבנה היסודי של קבצי טקסט משתלב במבנה ה "Universal Coded Character Set" (UCS) המשומש על ידי סטנדרט ISO 10646. הקבצים מורכבים מבית הכולל 21 סימבולים שונים שמוגדר