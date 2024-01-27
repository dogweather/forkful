---
title:                "עבודה עם JSON"
date:                  2024-01-19
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
JSON, שמשמעו JavaScript Object Notation, זהו פורמט מובנה לשיתוף נתונים בין שרת ללקוח או בין מערכות שונות. תכנתים שמתעסקים איתו כי זה פשוט, נקרא וכתוב על ידי מחשבים, וגם על ידי בני אדם.

## איך לעשות:
קריאה של JSON:
```Swift
import Foundation

let jsonString = """
{
    "name": "Jon Doe",
    "age": 30
}
"""

if let jsonData = jsonString.data(using: .utf8) {
    do {
        let person = try JSONSerialization.jsonObject(with: jsonData, options: []) as? [String: Any]
        if let name = person?["name"] as? String, let age = person?["age"] as? Int {
            print("Name: \(name), Age: \(age)")
        }
    } catch {
        print("JSON decoding failed: \(error)")
    }
}
```
תוצאה:
```
Name: Jon Doe, Age: 30
```

כתיבה של JSON:
```Swift
import Foundation

let person = [
    "name": "Jon Doe",
    "age": 30
] as [String : Any]

do {
    let jsonData = try JSONSerialization.data(withJSONObject: person, options: [])
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        print(jsonString)
    }
} catch {
    print("JSON encoding failed: \(error)")
}
```
תוצאה:
```
{"name":"Jon Doe","age":30}
```

## עומק טכני
JSON הוא שיטה שהתפתחה בשנים הראשונות של האינטרנט אבל הפכה לפופולרית בעיקר עם הזרמת אפליקציות רשת. באופן היסטורי, XML היה הבחירה הנפוצה יותר עבור מחליפי נתונים אבל JSON הפך למועדף בזכות קריאותו ופשטותו. ב-Swift, מחלקת `JSONSerialization` ב-Foundation framework מאפשרת לממש את הסידור והפיענוח של JSON בקלות. אפשרות נוספת היא מחלקת `JSONEncoder` ו-`JSONDecoder` שמספקות דרך נוחה יותר לסידור ופיענוח של הנתונים אל ומה-JSON באמצעות מודלים.

## ראו גם
- [מדריך Swift לסידור ופיענוח JSON](https://developer.apple.com/documentation/foundation/jsonserialization)
- [JSON ב-Swift עם JSONDecoder ו-Codable](https://developer.apple.com/documentation/foundation/archives_and_serialization/encoding_and_decoding_custom_types)
