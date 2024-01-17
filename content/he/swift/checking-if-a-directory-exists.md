---
title:                "בדיקת קיומו של ספרייה במחשב"
html_title:           "Swift: בדיקת קיומו של ספרייה במחשב"
simple_title:         "בדיקת קיומו של ספרייה במחשב"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

"מה ולמה?": בדיקת האם תיקייה קיימת היא פעולה שמאפשרת למתכנתים לבדוק אם תיקייה מסוימת קיימת במערכת הקבצים של האפליקציה. היא חשובה כדי לוודא שהקוד נכתב היטב ולמנוע תקלות בזמן ריצת האפליקציה.

"איך לעשות?": כדי לבדוק אם תיקייה קיימת, ניתן להשתמש בפונקציה פנימית של שפת Swift "FileManager.default.fileExists(atPath:)" ולמסור את הנתיב של התיקייה. הפונקציה תחזיר "true" אם התיקייה קיימת ו-"false" אם היא לא קיימת. לדוגמא: 
```Swift
if FileManager.default.fileExists(atPath: "/Users/Desktop") {
  print("התיקייה קיימת")
} else {
  print("התיקייה לא נמצאת")
}
```

"כיוון עמוק": בדיקת קיומו של תיקייה היא חלק מהתתחום הגדול של ניהול קבצים ותיקיות במערכת ההפעלה. בעבר, בשפת C הייתה פונקציה פנימית נפוצה בשם "opendir()" ששימשה לבדוק אם תיקייה קיימת, אך בשפת Swift נעשה שיפור באופן הרצף וניתן להשתמש בפונקציה הפנימית שאותה זכרנו קודם. בנוסף, ניתן גם להשתמש בפרמטרים נוספים כמו "followSymlinks" כדי לבחור אם יש לעקוב אחרי קישורים סמליים.

"ראה גם": למידע נוסף על ניהול קבצים ותיקיות בשפת Swift, ניתן לבדוק את הקישורים המובאים להלן:
- "How to Manage Files and Directories in Swift": https://www.ralfebert.de/ios/tutorials/filemanager-directory-ios-apps/
- "Working with Directories in Swift": https://www.avanderlee.com/swift/directory-structure-filemanager-swift/
- "The Importance of Proper File and Directory Management in Programming": https://medium.com/@0xboiler/file-and-directory-management-in-software-development-a9b3c4f99606