---
title:                "עבודה עם JSON"
aliases: - /he/swift/working-with-json.md
date:                  2024-02-03T19:24:35.814430-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
