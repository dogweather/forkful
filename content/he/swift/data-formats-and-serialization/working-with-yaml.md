---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:31.568694-07:00
description: "YAML, \u05E9\u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\u05D5 \"YAML Ain't\
  \ Markup Language\" (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\
  \u05DE\u05D5\u05DF), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05E1\u05D9\u05D3\u05D5\
  \u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D9\u05D3\u05D9\u05D3\u05D5\u05EA\
  \u05D9 \u05DC\u05D0\u05D3\u05DD \u05DC\u05DB\u05DC \u05E9\u05E4\u05D5\u05EA \u05D4\
  \u05EA\u05DB\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05D9\u05DD \u05E0\
  \u05D5\u05D4\u05D2\u05D9\u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5\
  \ \u05DC\u05E7\u05D1\u05E6\u05D9\u2026"
lastmod: '2024-02-25T18:49:38.180665-07:00'
model: gpt-4-0125-preview
summary: "YAML, \u05E9\u05DE\u05E9\u05DE\u05E2\u05D5\u05EA\u05D5 \"YAML Ain't Markup\
  \ Language\" (YAML \u05D0\u05D9\u05E0\u05D5 \u05E9\u05E4\u05EA \u05E1\u05D9\u05DE\
  \u05D5\u05DF), \u05D4\u05D5\u05D0 \u05EA\u05E7\u05DF \u05E1\u05D9\u05D3\u05D5\u05E8\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D9\u05D3\u05D9\u05D3\u05D5\u05EA\u05D9\
  \ \u05DC\u05D0\u05D3\u05DD \u05DC\u05DB\u05DC \u05E9\u05E4\u05D5\u05EA \u05D4\u05EA\
  \u05DB\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05D9\u05DD \u05E0\u05D5\
  \u05D4\u05D2\u05D9\u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D5 \u05DC\
  \u05E7\u05D1\u05E6\u05D9\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
---

{{< edit_this_page >}}

## מה ולמה?
YAML, שמשמעותו "YAML Ain't Markup Language" (YAML אינו שפת סימון), הוא תקן סידור נתונים ידידותי לאדם לכל שפות התכנות. תכנתיים נוהגים להשתמש בו לקבצי תצורה, הודעות בין-תהליכיות ואחסון נתונים מכיוון שהתמצאותו הרבה יותר קרובה לאנגלית פשוטה בהשוואה לפורמטים אחרים כמו XML או JSON, מה שהופך אותו לפשוט יותר להבנה ולכתיבה.

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
