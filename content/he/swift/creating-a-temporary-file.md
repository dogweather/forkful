---
title:    "Swift: יצירת קובץ זמני"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## עבודה ב-Swift בשביל מתחילים: יצירת קובץ זמניים

## Why:
מתכנתים שמתחילים ללמוד Swift לעתים קרובות נתקלים בהצורך ליצור קבצים זמניים (temp files). אבל למה בעצם אפשר לצור קובץ זמני ומה יהיה השימוש שלו? קובץ זמני הוא קובץ שנוצר במטרה לפעול כדוגמאל מחזיק נתונים זמנית. כאשר הוא יוצר נדבק בקוד שלך ונמחק. השימוש התקין ב&amp; Swift בשביל קבצי זמןי היא כאשר אתה רוצה לבצע פעולה מסוימת על נתונים אך אין לך צורך לשמור אותם לתקופה ארוכה.

## How To:
```Swift
func createTempFile() {
    let filePath = NSTemporaryDirectory().stringByAppendingPathComponent("file.txt") // נתיב לקובץ בספריית הזמניים
    let fileContent = "תוכן פעמונים קטן 😊" // יצירת התוכן של הקובץ כמחרוזת
    do {
        try fileContent.writeToFile(filePath, atomically: true, encoding: NSUTF8StringEncoding) // כתיבת התוכן לתוך הקובץ
    } catch {
        // איזה אירוע יכול לגרום לכשלון ביצירת הקובץ?
        print(error)
    }
}
```

כאשר תרצה לקרוא את תוכן הקובץ אתה יכול לעשות זאת כך:
```Swift
func readTempFile() {
    let filePath = NSTemporaryDirectory().stringByAppendingPathComponent("file.txt") // נתיב לקובץ בספריית הזמניים
    let fileContent = try? String(contentsOfFile: filePath, encoding: NSUTF8StringEncoding) // קריאת התוכן מתוך הקובץ כמחרוזת
    print(fileContent)
}
```

## Deep Dive:
הוספתמייצבות ⌥ Opt במקרים של תקינה ריאלית, אלא אתה רוצה ליצור קובץ זמני זמני שבו אתה יכול לבדוק את התוכן שלו במהלך פיתוח האפליקציה שלך. אם אחר כך, אתה יכול להשתמש בקובץ זמני במטרה לבדוק ולבדוק תפוקת פיתוח שלך ולפר