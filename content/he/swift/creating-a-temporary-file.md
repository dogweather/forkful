---
title:                "יצירת קובץ זמני"
html_title:           "Swift: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת קובץ זמני היא תהליך בו מתכנתים יוצרים קובץ שמשמש לפעולות זמניות ומתנגשות במערכת האחסון. הקובץ משמש למטרות כמו חינוך בפתרון בעיות בעת איתור בעיות בתוכנה או כאמצעי לצבור נתונים בזמן הרצת התוכנית.

## איך לעשות:

```Swift 
let tempFile = try? FileManager.default.url(for: .itemReplacementDirectory, in: .userDomainMask, appropriateFor: nil, create: true).appendingPathComponent("tempfile.txt")
try? "This is a temporary file!".write(to: tempFile!, atomically: true, encoding: .utf8)
```

### תוצאה:

בתיקיית הפרויקט, קובץ בשם "tempfile.txt" יוצר בתוך התיקייה המיועדת לקבצי זמניים (itemReplacementDirectory) תחת מחלקת המשתמש (userDomainMask) במערכת ההפעלה. הקובץ ישמור על התוכן שנכתב באמצעות פקודת הכתיבה (write) ולאחר מכן יימחק מהמערכת כשתוכנית ה-Swift תמצא.

## חקירה עמוקה:

יצירת קובץ זמני היא תהליך שנתמך על ידי מערכות הפעלה כמו macOS ו-Windows כדי לאפשר למתכנתים לבצע פעולות מתנגשות במערכת האחסון בזמן הריצה. יתר על כן, ישנן אלטרנטיבות ליצירת קבצים זמניים באמצעות פקודות ספציפיות בשפת Swift עצמה.

כאשר מיזוגת, תהליך היצירה של קבצים זמניים כולל יצירת מבנה מדומה של קובץ, כתיבת התוכן לקובץ זה ולאחר מכן מחיקתו מהמערכת. דבר זה יכול לגרום לזיכרון חשוב עבור תוכניות גדולות ומורכבות בהמשך הריצה שלהן.

## ראה גם:

- [ניתוח יישומי זמות ליצירת קובץ מחדש](https://www.usenix.org/legacy/event/usenix2000/invited_talks/mckusick/tsld014.htm)
- [מאמנו של לורן עולם התכנות: "יישומי זמות נכונים בכמה דקות"](https://www.youtube.com/watch?v=fTlJkzflBTE)