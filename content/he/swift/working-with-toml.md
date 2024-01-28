---
title:                "עבודה עם TOML"
date:                  2024-01-26T04:27:13.465910-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-toml.md"
---

{{< edit_this_page >}}

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
