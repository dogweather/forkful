---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:35.814430-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1-Swift \u05E4\
  \u05D9\u05E8\u05D5\u05E9\u05D4 \u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA\
  \ \u05E2\u05DD \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05E7\u05DC\u05D9\u05DC \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\
  \u05E9\u05D9\u05DD \u05D1-JASON \u05DC\u05E9\u05DC\u05D9\u05D7\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D1\u05D9\u05DF \u05E9\u05E8\u05EA \u05DC\u05D0\u05E4\
  \u05DC\u05D9\u05E7\u05E6\u05D9\u05D4 \u05D5\u05D5\u05D1 \u05DE\u05E9\u05D5\u05DD\
  \ \u05E9\u05D4\u05D5\u05D0 \u05E7\u05E8\u05D9\u05D0 \u05D5\u05E7\u05DC\u2026"
lastmod: '2024-03-13T22:44:39.941115-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON \u05D1-Swift \u05E4\u05D9\
  \u05E8\u05D5\u05E9\u05D4 \u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\
  \u05DD \u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E7\
  \u05DC\u05D9\u05DC \u05DC\u05D4\u05D7\u05DC\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

## מה ולמה?

עבודה עם JSON ב-Swift פירושה התמודדות עם פורמט נתונים קליל להחלפת נתונים. תכנתים משתמשים ב-JASON לשליחת נתונים בין שרת לאפליקציה ווב משום שהוא קריא וקל לניתוח עבור בני אדם ומכונות.

## איך ל:

Swift מפשטת את ניתוח ה-JSON עם הפרוטוקול `Codable`. הנה איך לפענח JSON לאובייקט של Swift:

```Swift
import Foundation

// הגדרת מודל שמיישם את Codable
struct User: Codable {
    var name: String
    var age: Int
}

// מחרוזת JSON
let jsonString = """
{
    "name": "John Doe",
    "age": 30
}
"""

// המרת מחרוזת JSON ל-Data
if let jsonData = jsonString.data(using: .utf8) {
    // פענוח נתוני JSON לאובייקט משתמש
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Name: \(user.name), Age: \(user.age)")
    } catch {
        print("Error decoding JSON: \(error)")
    }
}
```

דוגמא לפלט:
```
Name: John Doe, Age: 30
```

## צלילה עמוקה

JSON (פורמט אובייקטים של JavaScript) זכה לקבלת פנים רחבה מאז שנות ה-2000 המוקדמות, לאחר שדאגלס קרוקפורד קבע אותו. הוא החליף את XML במקרים רבים בשל התחביר הפשוט יותר והביצועים הטובים יותר. בעוד ש-`Codable` של Swift הוא הבחירה הראשונה ל-JSON, קיימות אלטרנטיבות כמו `JSONSerialization` למקרים בהם מתמודדים עם סוגים שאינם תואמים ל-Codable. מאחורי הקלעים, `Codable` ממספר את ניתוח הרמה הנמוכה והופך את הסריאליזציה/דה-סריאליזציה לחלקה.

## ראה גם

- חקור עוד על JSON ו-Swift בבלוג הרשמי של Swift: [Swift.org](https://swift.org/blog/)
- בדוק את התיעוד של `Codable`: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- למבנים מורכבים של JSON, שקול להשתמש בספריות צד שלישי כמו SwiftyJSON, הזמינות ב-[GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
