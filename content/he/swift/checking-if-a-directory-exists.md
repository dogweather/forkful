---
title:    "Swift: בדיקת קיום תיקייה במחשב"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

בתוכנות המחשב, לפעמים נצטרך לוודא כמה פרטים על תיקיות שונות במערכת הקבצים. יתכן כי נרצה לוודא אם תיקייה קיימת או לא, כדי לבצע פעולות מתאימות בהתאם. לדוגמה, ניתן להשתמש בזה כדי לבדוק אם תיקיית ההורדות שלנו קיימת לפני שאנו מנסים להוריד קובץ לתוכה.

## איך לבדוק אם תיקייה קיימת

לבדיקת קיומה של תיקייה, ניתן להשתמש בפונקציה `fileExists(atPath:)` שמקבלת כפרמטר את הנתיב של התיקייה שרוצים לבדוק. בפונקציה זו, יש להחזיר `true` אם התיקייה קיימת ו-`false` אם לא. ניתן גם להשתמש בפונקציה `fileExists(atPath: isDirectory:)` שמחזירה כמו כן בעל תיקייה או לא.

```Swift
let fileManager = FileManager.default
let downloadsPath = NSSearchPathForDirectoriesInDomains(.downloadsDirectory, .userDomainMask, true)[0]
let downloadsFolderExists = fileManager.fileExists(atPath: downloadsPath)
print("תיקיית הורדות קיימת: \(downloadsFolderExists)")
// תעודת הורדות קיימת: true
```

```Swift
let fileManager = FileManager.default
let documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0]
var isDirectory: ObjCBool = false
let documentsFolderExists = fileManager.fileExists(atPath: documentsPath, isDirectory: &isDirectory)
print("תיקיית מסמכים קיימת: \(documentsFolderExists)")
print("האם תיקיית מסמכים הוא תיקייה? \(isDirectory.boolValue)")
// תיקיית מסמכים קיימת: true
// האם תיקיית מסמכים הוא תיקייה? true
```

## העמקה נוספת

לבדיקת קיומה של תיקייה ניתן גם להשתמש בפונקציה `fileExists()` שמקבלת כפרמטר את הנתיב של התיקייה ומוסיפה גם פרמטרים נוספים שמתארים מה התיקייה היא (עבור `isDirectory`) ועבור מי היא (עבור `withIntermediateDirectories`). ניתן למצוא עוד מידע על הפונקציה הזו ו