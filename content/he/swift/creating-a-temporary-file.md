---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
יצירת קובץ זמני היא בסיסית פעולה של שמירה של נתונים בצורה זמנית. מתכנתים משתמשים בזה כאשר הם צריכים לשמור נתונים בזמנית, לא משנה מה הסיבה: החזר אחורה, שמירה של גרסאות, שמירה של מידע ביניים ועוד. 

## איך לגרום לזה לקרות:
הנה קוד Swift שמציג איך ליצור קובץ זמני:

```Swift
import Foundation

let tempDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let tempFileURL = tempDirectoryURL.appendingPathComponent(UUID().uuidString)

do {
    try "Hello, Swift!".write(to: tempFileURL, atomically: true, encoding: .utf8)
    print("Saved to \(tempFileURL)")
} 
catch {
    print("Error saving to temporary file: \(error)")
}
```

פלט דוגמה שתראה כמו זה:

```Swift
Saved to file:///private/var/folders/xx/xxxxxx/T/48B5A8A2-xxxx-4DA9-xxxx-90EBE25566C9
```

## Deep Dive
בעבר, השימוש בקבצים זמניים היה חיוני מאוד בגלל הזכרון המוגבל של המחשבים. היום, עם ההתפתחות של החומרה, חלק מהצורך שאיבד החשיבות שלו. אף על פי כן, הם עדיין משמשים ביצירה של גרסאות, חזרה אחורה, ובמקרים שבהם יש טעות שאין אפשרות לתקן.

חלופה לקבצים זמניים היא שימוש ב-memory cache או במודלים של נתונים בזיכרון. הבחירה בין השיטות תלויה בהקשר ובדרישות של האפליקציה.

## ראה גם
הידעתם? ברזרברים בחרו את המינים טמפורריות: [למאמר](https://en.wikipedia.org/wiki/Temporary_file)

דוקומנטציה של אפל מאוד מפורטת על `URL`: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/url)

דוקומנטציה של אפל: [NSTemporaryDirectory](https://developer.apple.com/documentation/foundation/1413044-nstemporarydirectory)

רצון לייעיל את הקוד: [Cache Policies](https://developer.apple.com/documentation/foundation/nsurlrequest/cachepolicy)