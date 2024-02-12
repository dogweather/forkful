---
title:                "כתיבה לשגיאה התקנית"
aliases: - /he/swift/writing-to-standard-error.md
date:                  2024-02-03T19:35:15.067738-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לשגיאת פלט סטנדרטית (stderr) עוסקת בהפניית הודעות השגיאה או הפלט הדיאגנוסטי של התוכנית שלך לזרם נפרד, שונה מהפלט הסטנדרטי (stdout). זה קריטי לניפוי באגים ולתיעוד שגיאות מבלי לטעון את הפלט הסטנדרטי, מקל על הבנת המפתחים והמשתמשים של מצב התוכנית והבעיות שלה.

## איך לעשות:

ב-Swift, ניתן לכתוב לשגיאת פלט סטנדרטית באמצעות המחלקה `FileHandle` לגישה ישירה ל-stderr. הנה דוגמה פשוטה:

```swift
import Foundation

// הגדרת הודעה
let errorMessage = "אירעה שגיאה.\n"

// המרת ההודעה לנתונים
if let data = errorMessage.data(using: .utf8) {
    // כתיבת הודעת השגיאה ל-stderr
    FileHandle.standardError.write(data)
}
```

פלט ל-stderr (שנצפה לרוב בקונסולה או בטרמינל):
```
אירעה שגיאה.
```

לצורך תיעוד מורכב יותר או כאשר עובדים עם ספריות חיצוניות, קיים האפשרות לשקול שימוש בספריית צד שלישי כמו **SwiftLog**. למרות ש-**SwiftLog** לא כותב ל-stderr באופן ישיר מחוץ לקופסה, ניתן ליישם backend לוגים מותאם אישית כדי להשיג זאת. הנה דוגמה פשוטה להגדרת מטפל בלוגים מותאם אישית שכותב ל-stderr:

תחילה, הוסף את **SwiftLog** לתלות הפרויקט שלך ב-`Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

לאחר מכן, יש ליישם מטפל לוגים מותאם אישית שכותב ל-stderr:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// שימוש
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("זו הודעת שגיאה")
```

פלט ל-stderr:
```
זו הודעת שגיאה
```

מטפל המותאם אישית הזה מאפשר לך למסר הודעות שגיאה של SwiftLog ישירות לשגיאת פלט סטנדרטית, משתלב בצורה חלקה עם הודעות לוג אחרות שהיישום שלך עשוי לייצר.
