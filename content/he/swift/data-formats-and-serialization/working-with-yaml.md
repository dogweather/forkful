---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:31.568694-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Swift \u05DC\u05D0\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\u05D1\
  \u05E0\u05D9\u05EA \u05DC\u05E4\u05E2\u05E0\u05D5\u05D7 \u05D5\u05D4\u05E1\u05E8\
  \u05D4 \u05E4\u05DC\u05D8\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D1-YAML,\
  \ \u05DE\u05D4 \u05E9\u05DE\u05D7\u05D9\u05D9\u05D1 \u05E9\u05D9\u05DE\u05D5\u05E9\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\
  \u05D9. \u05D1\u05D7\u05D9\u05E8\u05D4 \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\
  \u05EA \u05D4\u05D9\u05D0 `Yams`, \u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05DC\u05E2\
  \u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML\u2026"
lastmod: '2024-03-13T22:44:39.939456-06:00'
model: gpt-4-0125-preview
summary: "Swift \u05DC\u05D0 \u05DB\u05D5\u05DC\u05DC\u05EA \u05EA\u05DE\u05D9\u05DB\
  \u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05E4\u05E2\u05E0\u05D5\u05D7\
  \ \u05D5\u05D4\u05E1\u05E8\u05D4 \u05E4\u05DC\u05D8\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD \u05D1-YAML, \u05DE\u05D4 \u05E9\u05DE\u05D7\u05D9\u05D9\u05D1 \u05E9\
  \u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3\
  \ \u05E9\u05DC\u05D9\u05E9\u05D9."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
Swift לא כוללת תמיכה מובנית לפענוח והסרה פלטת נתונים ב-YAML, מה שמחייב שימוש בספריות צד שלישי. בחירה פופולרית היא `Yams`, ספרייה לעבודה עם YAML ב-Swift.

ראשית, אתה צריך להוסיף `Yams` לפרויקט שלך. אם אתה משתמש ב-Swift Package Manager, תוכל להוסיף אותו כתלות בקובץ `Package.swift` שלך:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", from: "4.0.0")
]
```

### פענוח YAML ל-Swift
נניח שיש לך את התצורה YAML הבאה לאפליקציה פשוטה:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

הנה איך תוכל לפענח מחרוזת YAML זו ב-Swift באמצעות `Yams`:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // דוגמה לגישה לנתונים שנפענחו
        if let name = data["name"] as? String {
            print("שם האפליקציה: \(name)")
        }
    }
} catch {
    print("שגיאה בפענוח YAML: \(error)")
}
```

פלט לדוגמה:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
שם האפליקציה: MyApp
```

### הסרה פלטת של אובייקטי Swift ל-YAML
המרת אובייקט של Swift חזרה למחרוזת YAML היא גם קלה עם `Yams`. נניח שיש לך את אותה מבנה נתונים שצריך להיות מוסר פלט:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("שגיאה בהסרה פלטת ל-YAML: \(error)")
}
```

זה יפיק מחרוזת בפורמט YAML:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

דוגמאות אלה מדגימות פעולות בסיסיות לעבודה עם YAML ביישומי Swift. זכור, בעוד ש-YAML מצטיין בקריאות לאדם ובנוחות שימוש, תמיד שקול את הצרכים הספציפיים של היישום שלך, במיוחד לגבי ביצועים ומורכבות, כאשר אתה בוחר את פורמט הסידור הנתונים שלך.
