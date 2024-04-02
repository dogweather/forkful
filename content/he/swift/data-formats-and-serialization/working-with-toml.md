---
date: 2024-01-26 04:27:13.465910-07:00
description: "TOML (Tom's Obvious, Minimal Language) \u05D4\u05D5\u05D0 \u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05E1\u05D9\u05D3\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05E9\u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D1\u05D6\u05DB\u05D5\
  \u05EA \u05D4\u05E1\u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\
  \u05E8\u05D4 \u05E9\u05DC\u05D5. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-TOML \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D1\
  \u05E6\u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05E9\u05D1\u05D4\u05DD \u05E7\u05E8\
  \u05D9\u05D0\u05D5\u05EA \u05E2\u05DC\u2026"
lastmod: '2024-03-13T22:44:39.944286-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language) \u05D4\u05D5\u05D0 \u05E4\u05D5\u05E8\
  \u05DE\u05D8 \u05E1\u05D9\u05D3\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05E9\u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D1\u05D6\u05DB\u05D5\u05EA\
  \ \u05D4\u05E1\u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\u05E8\
  \u05D4 \u05E9\u05DC\u05D5. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1-TOML \u05E2\u05D1\u05D5\u05E8 \u05E7\u05D1\u05E6\
  \u05D9 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05E9\u05D1\u05D4\u05DD \u05E7\u05E8\u05D9\
  \u05D0\u05D5\u05EA \u05E2\u05DC\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

## מה ולמה?
TOML (Tom's Obvious, Minimal Language) הוא פורמט סידור נתונים שקל לקרוא בזכות הסמנטיקה הברורה שלו. תכנתים משתמשים ב-TOML עבור קבצי תצורה שבהם קריאות על ידי בני אדם וניתוח קל על ידי מכונות הם מפתח.

## איך ל:
להתחיל, אתם צריכים מפענח TOML. ב-Swift אין מפענח מובנה, אז בואו נשתמש ב-`TOMLDecoder`. התקינו אותו דרך Swift Package Manager ואז תעשו סידור ופירוק של TOML בקלות.

```Swift
import TOMLDecoder

let tomlString = """
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Title: \(config.title), Owner: \(config.owner.name), DOB: \(config.owner.dob)")
    } catch {
        print("שגיאה בניתוח TOML: \(error)")
    }
}
```

הקוד הזה מפיק:
```
Title: TOML Example, Owner: Tom Preston-Werner, DOB: 1979-05-27 07:32:00 +0000
```

## עיון מעמיק
TOML תוכנן על ידי טום פרסטון-ורנר, שותף מייסד של GitHub, כחלופה ידידותית יותר לאנשים לפורמטים כמו JSON או YAML. המטרה שלו היא להגיע לבהירות, להקטין את הסיכויים לפרשנות שגויה על ידי בן אדם או מכונה. בנוגע לחלופות, YAML ו-JSON הם החשודים הרגילים, עם YAML המוטה יותר לקריאות על ידי בני אדם ו-JSON כאפשרות היותר פשוטה הידידותית למכונה. כשעובדים עם TOML ב-Swift, אין לנו מפענח מקורי. עם זאת, ספריות צד שלישי כמו `TOMLDecoder` מקלות על המרה קלה בין מחרוזות TOML לסוגים ב-Swift, במיוחד דרך פרוטוקולי `Codable` שהוצגו ב-Swift 4 ששיפרו את הסידור.

## ראו גם
- התקן של TOML: https://toml.io
- GitHub עבור `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- תיעוד Swift על `Codable`: https://developer.apple.com/documentation/swift/codable
- השוואה של פורמטים לסידור נתונים: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
