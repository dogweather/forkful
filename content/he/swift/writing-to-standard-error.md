---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה כתיבה לשגיאה סטנדרטית, ולמה זה נחוץ? במילים פשוטות, זה עניין של כתיבה לפלט שמיועד לשגיאות ולא לפלט רגיל. זה עוזר לפרק בין הודעות שגיאה לבין פלט תקני, במיוחד כשאתה משתמש בפלט של תוכנה בסקריפטים או ביצירת לוגים.

## How to:
```Swift
import Foundation

// דוגמה של כתיבה ל-stderr
let stderr = FileHandle.standardError
if let data = "שגיאה: משהו השתבש.\n".data(using: .utf8) {
    stderr.write(data)
}

// דוגמה של כתיבה ל-stdout להשוואה
print("פלט רגיל: הפעולה הצליחה.")

// הדפסה של ההפרדה בשימוש בפקודות בשל
// swift myScript.swift > stdout.txt 2> stderr.txt
```
אם נריץ את הקוד הזה, "שגיאה: משהו השתבש." יופיע בקובץ stderr.txt ו-"פלט רגיל: הפעולה הצליחה." בstdout.txt.

## Deep Dive:
מאז ימי UNIX, שימוש בפלט שגיאה סטנדרטית זה דרך להפריד בין ההודעות השונות של התכנית. בעולמות אחרים, כמו Windows, גם יש תמיכה בפלט שגיאה סטנדרטית. אלטרנטיבות כוללות כתיבה לקובץ לוג או שימוש בספריות של יומן אירועים. פרטי המימוש בSwift משתמשים ב-api של UNIX כדי להתעסק עם ה- file handles.

## See Also:
- [Apple Developer Documentation on FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- [Swift.org Documentation](https://www.swift.org/documentation/)
- [GNU Error Reporting Documentation](https://www.gnu.org/software/libc/manual/html_node/Error-Messages.html)
