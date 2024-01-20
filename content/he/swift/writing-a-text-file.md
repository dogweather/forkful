---
title:                "כתיבת קובץ טקסט"
html_title:           "Swift: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא פעולה שמאפשרת למתכנתים ליצור ולשמור מידע בקובץ טקסט פשוט. כתיבה זו מאפשרת שמירת מידע מבוקר ושינוי מידע לכל תוכנית בצורה תמידית ומרוכזת. תהליך זה מאפשר למתכנתים לנהל ולעבוד עם נתונים בצורה יעילה ומסודרת.

## איך לעשות זאת?
```swift
// כל המתכנתים אוהבים מחרוזות
let myString = "שלום עולם!"

// כדי לכתוב קובץ טקסט, נשתמש בפונקציה המובנית לכתיבת קבצים
if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = dir.appendingPathComponent("myFile.txt")

    // נשתמש במחרוזת שיצרנו לפני כדי לכתוב את המידע לקובץ
    do {
        try myString.write(to: fileURL, atomically: false, encoding: .utf8)
        print("קובץ טקסט נוצר בהצלחה!")
    } catch {
        print(error.localizedDescription)
    }
}
```
כאן, אנו משתמשים במחרוזת פשוטה כדי ליצור מידע ולכתוב אותה לקובץ טקסט באמצעות פונקצית ```write(to:atomically:encoding:)```. ניתן לשנות את המידע שנכתוב לקובץ תוך שימוש במחרוזת אחרת או על ידי הוספת קוד לכתיבת מידע ממקורות אחרים.

## הכנסה לתהומות
לכתיבת קובץ טקסט יש יישומים רבים בתחום התכנות. היא מאפשרת למתכנתים לשמור ולנהל מידע בצורה יעילה ומאורגנת, והיא משמשת כבנסיבית כאשר נדרש לעבוד עם מספר נתונים גדול לאורך זמן.

## ראו גם
למידע נוסף על קריאת וכתיבת קבצי טקסט בשפת סוויפט, הסתכלו על המקורות הבאים:

- [ספריית ה-FileManager של אפל](https://developer.apple.com/documentation/foundation/filemanager)
- [רפרנס לשפת סוויפט](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)