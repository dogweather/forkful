---
title:    "Swift: יצירת קובץ זמני"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני נחשבת כלי חשוב בתכנות ב-Swift, מאחר וזהו דרך נוחה ובטוחה לשמור נתונים בזמן ריצת התוכנית.

## כיצד לעשות זאת

אם ברצונכם ליצור קובץ זמני ב-Swift, תוכלו לעשות זאת בקלות עם הפונקציה `NSTemporaryDirectory()`. הנה דוגמה קצרה שתלמד אתכם איך ליצור קובץ זמני ולשמור בו נתונים:

```Swift
let tempDirectoryPath = NSTemporaryDirectory()
let tempFileURL = URL(fileURLWithPath: tempDirectoryPath).appendingPathComponent("myTempFile.txt")

let dataToWrite = "Hello World!".data(using: .utf8)
try? dataToWrite?.write(to: tempFileURL)
```

ניתן לראות בדוגמא הזאת שאנו משתמשים בפונקציה `NSTemporaryDirectory()` כדי לקבל את הנתיב של התיקייה הזמנית, ואז משתמשים בפונקציה `URL(fileURLWithPath:)` כדי ליצור URL לקובץ זמני. לבסוף, אנו משתמשים בפונקציה `write(to:)` כדי לשמור את הנתונים בקובץ.

## עיון מעמיק

יצירת קובץ זמני ב-Swift יכולה להיות יעילה מאוד, מתוך כמה סיבות:

- קבצים זמניים משמשים כדי לשמור נתונים שאינם דרושים לתקופה ארוכה, לכן ניתן למחוק אותם כשעולה צורך במקום לנהלם לאורך זמן.
- ביצוע רשימת פעולות על קובץ זמני נחשב אחריות יותר, מכיוון שעם סיום התוכנית הכל נמחק. אין צורך לדאוג לכך שנתונים או תכנים יישארו במקום לאורך זמן ויכולים לגרום לבעיות בעתיד.

## ראו גם

- [מדריך: יצירת קובץ זמני ב-Swift](https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-file-in-swift)
- [תיעוד: NSTemporaryDirectory()](https://developer.apple.com/documentation/foundation/1409296-nstemporarydirectory)
- [תיעוד: URL(fileURLWithPath:)](https://developer.apple.com/documentation/foundation/url/312