---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא תהליך שמאפשר לשמור נתונים כטקסט במערכת הקבצים. מתכנתים עושים את זה כדי לשמור הגדרות, לוגים, וכל נתון שצריך לאחסן לטווח ארוך או לשיתוף עם תהליכים/אנשים.

## איך לעשות:
```Swift
import Foundation

// כאן מגדירים נתיב לקובץ
if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = dir.appendingPathComponent("example.txt")
    
    // כאן אנו כותבים את הטקסט לקובץ
    let text = "שלום, זה הטקסט שלנו"
    do {
        try text.write(to: fileURL, atomically: false, encoding: .utf8)
        print("טקסט נכתב בהצלחה")
    } catch {
        print("התרחשה שגיאה בכתיבה לקובץ: \(error)")
    }
}
```

## עומק של ידיעה
בעבר, שפות תכנות כמו C השתמשו ב-FILE pointers ובפונקציות כמו `fopen`, `fwrite`, ו-`fclose` לניהול קבצים. ב-Swift, עם פריימוורק Foundation של Apple, יש API מודרני שמחביא את רוב הפרטים הנמוכים ומאפשר כתיבה וקריאה בצורה נוחה יותר. חלופות נפוצות הן שימוש במסדי נתונים או בענן לאחסון נתונים.

## ראו גם:
- [תיעוד Swift הרשמי](https://docs.swift.org/swift-book/)
