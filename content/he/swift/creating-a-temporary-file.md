---
title:                "Swift: יצירת קובץ זמני"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## על מי

יצירת קובץ זמני יכול להיות מועילה כאשר אנחנו צריכים ליצור ולשדרג קבצים במהלך תהליך מתפתח או כאשר אנחנו מבנהים מערכות פטנט.

## איך לעשות

כאשר אנחנו יוצרים קובץ זמני בעזרת פרוטוקול `NSTemporaryDirectory ()` ומתודה `URLByAppendingPathComponent`:

```Swift
let tempDir = NSTemporaryDirectory()
let tempURL = URL(fileURLWithPath: tempDir)
    .appendingPathComponent("myTempFile")
    .appendingPathExtension("txt")
print(tempURL.path)
```

כאשר נריץ קוד זה, יתווסף לנו קובץ זמני במחיצת /tmp עם שם הקובץ "myTempFile.txt" וכתובת URL מלאה לקובץ זה תופיע במסך.

## העמקה

כאשר אנחנו יוצרים קובץ זמני עם שם קובץ ייחודי, הקובץ יתווסף למחיצת /tmp עם השם שציינו. תוכלו גם לבחור לשנות את התוסף שצמחנו לקובץ זמני עם `appendingPathExtension` לתוסף אחר או לשנות את המיקום של הקובץ עם `appendingPathComponent`.

## ראו גם

[מדריך לברירת המחדל של קבצים שמוחקים עצמם שבמחיצת הזמן ב-iOS](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html#//apple_ref/doc/uid/TP40010672-CH2-SW23)

[מדריך לשימוש במצב אמת ב-iOS שבו קבצים זמניים ישארו זמינים בכל המחיצות](https://www.hackingwithswift.com/example-code/foundation/how-to-create-a-temporary-file-the-right-way)