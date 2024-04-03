---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:15.067738-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Swift, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\u05E9\u05D2\u05D9\u05D0\
  \u05EA \u05E4\u05DC\u05D8 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DE\u05D7\u05DC\u05E7\u05D4 `FileHandle`\
  \ \u05DC\u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC-stderr. \u05D4\
  \u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4."
lastmod: '2024-03-13T22:44:39.932841-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Swift, \u05E0\u05D9\u05EA\u05DF \u05DC\u05DB\u05EA\u05D5\u05D1 \u05DC\
  \u05E9\u05D2\u05D9\u05D0\u05EA \u05E4\u05DC\u05D8 \u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8\u05D9\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DE\u05D7\
  \u05DC\u05E7\u05D4 `FileHandle` \u05DC\u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\u05D9\
  \u05E8\u05D4 \u05DC-stderr."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

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
