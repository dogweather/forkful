---
title:                "Swift: כתיבה לטעות תקן"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# למה

כתיבה לפלט השגיאות התקניים (standard error) היא כלי חשוב בתכנות בשפת סוויפט. הוא מאפשר למפתחים לתקן בעיות ולמצוא את מקורן של השגיאות בקוד שלהם בצורה מהירה ויעילה.

# כיצד לעשות זאת

תחילה, נצטרך לייבא את `Foundation` framework כדי להשתמש במודול "File Handle" שיאפשר לנו לגשת לכתיבת ה-Python של הפלט התקניים. לאחר מכן, נשתמש בפקודת `guard let` לבדיקת תקינות הארגומנט שאנו מעבירים לפונקציה `FileHandle.standardError`.

```Swift
import Foundation

guard let standardError = FileHandle.standardError else {
  fatalError("Unable to access standard error.")
}

print("This is a sample error message.", to: standardError)
```

הפלט המתקבל מהקוד שלו תהיה כזו:

```
This is a sample error message.
```

כעת, אם נרצה לשנות את שם הקובץ או את המיקום של הפלט התקניים, אנו יכולים לעשות זאת על ידי שימוש בפקודה `FileHandle.init(forWritingAtPath:)` ולהעביר לה את הנתיב הרלוונטי על מנת ליצור קובץ חדש לכתיבה.

```Swift
import Foundation

let fileHandle = FileHandle(forWritingAtPath: "error_log.txt")

guard let errorLog = fileHandle else {
  fatalError("Unable to create error log file.")
}

print("This is a sample error message.", to: errorLog)
```

תוכלו לראות את קובץ השגיאות שנוצר בנתיב שנמצאים בו הקבצים שלכם. אל תשכחו לסגור את הקובץ בסיום התהליך כדי לוודא שהמידע נשמר. כעת כאשר אתם מבינים את הבסיסים, אתם יכולים להתחיל להשתמש בכתיבה לפלט התקניים בקודים שלכם לתיעוד ולאיתור שגיאות.

# חקירה מעמיקה

ה-Python של הפלט התקניים משמש לכתיבה ל-Python של הפלט הרגיל (standard output), איך נדע להבחין ביניהם? ההבדל היחידי הוא בכתובת המפנה המועברת לפונ