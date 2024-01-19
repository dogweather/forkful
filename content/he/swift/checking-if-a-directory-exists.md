---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

---

## מה ולמה?

בדיקה אם ספרייה קיימת במערכת הקבצים, היא פעולה שבה התוכנה מסתכלת על המיקום שניתן לה ומחליטה האם יש בו ספריה או לא. בכחול השמים, מתכנתים מבצעים פעולה זו כדי לוודא שהקובץ שהם מחפשים יהיה שם בעת מה, או כדי למנוע שגיאות בעת יצירת ספריה חדשה.

## איך לעשות:

יש דרך קלה לבדוק אם מדובר בספריה ב־Swift. הנה קוד שיבדוק אם ספריה מסוימת קיימת:

```Swift
import Foundation

func isDirectoryExists(path: String) -> Bool {
    var isDirectory = ObjCBool(false)
    let exists = FileManager.default.fileExists(atPath: path, isDirectory: &isDirectory)
    return exists && isDirectory.boolValue
}

print(isDirectoryExists(path: "/User/Desktop")) // sample output: true or false
```
משתנה ה-output של הפונקציה הוא בוליאני שמציין האם הנתיב המבוקש הוא ספריה (true) או לא (false).

## נסיעה לעומק:

הפונקציה `fileExists(atPath:isDirectory:)` שאנו משתמשים בה לבדוק אם ספריה קיימת הוא חלק מ- `FileManager`, המשמש כממשק לתכנת לעבודה עם מערכת הקבצים של המחשב. המחלקה הזו תמיד הייתה חלק מניתוח פלטפורמה של Apple ואנחנו משתמשים בה המון בעבודה עם קבצים ונתיבים.

ייתכן שיהיו תחליפים אחרים לקוד הזה, כמו `NSFileManager` (מתוך Cocoa) במוסדות, אך `FileManager` הוא האופציה המועדפת לתכנות Swift המודרני.

## ראו גם:

- הנחיות למתכנתים של Apple ל- FileManager: [Link](https://developer.apple.com/documentation/foundation/filemanager)
- מתכנתים ב- StackOverflow מדברים על ההיסטוריה של FileManager והשימוש שלו: [Link](https://stackoverflow.com/questions/56495661/managing-files-and-directory-paths-using-filemanager-in-ios)