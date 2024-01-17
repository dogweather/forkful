---
title:                "עבודה עם json"
html_title:           "Swift: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/working-with-json.md"
---

{{< edit_this_page >}}

# מה זה ולמה?
JSON הוא תבנית נתונים מקורית עבור מסמכי טקסט, המתאימה לשימוש בתכנות. מסמכים מסוג זה משמשים כדי להעביר מידע בין שתי השפות כאשר ניתן לקרוא ולכתוב באספקטים שונים

קוראים JSON כדי לעבד מידע מקוון בשפות תכנות כמו Swift. הכי קל לעבד עם בדיקת מסמכים כדי לבצע מסמך חדש ולצרף פזורים

# כיצד לעבוד?
כדי לגשת למידע JSON לשימוש בפרויקט Swift, ניתן להשתמש בתוכנית קוד אשר מאפשרת יצירת אובייקטים נתונים בצורה נוחה ומיידית.

המהדר של Swift מאפשר לך להתמקד בעיבוד JSON ולתקשור עם מסמכי JSON בקלות ובמהירות.

הנה דוגמא של קוד Swift:
```
let jsonData = """
{
  "name": "John Doe",
  "age": 28,
  "hobbies": ["reading", "hiking", "painting"]
}
""".data(using: .utf8)

// יצירת אובייקט נתונים מהמידע בתצורת JSON
struct Person: Codable {
  var name: String
  var age: Int
  var hobbies: [String]
}

do {
  // שימוש במחלקה לקריאת המידע והמרתו לאובייקט נתונים
  let person = try JSONDecoder().decode(Person.self, from: jsonData)
  
  // הדפסת המידע מהאובייקט נתונים
  print(person.name) // John Doe
  print(person.age) // 28
  print(person.hobbies) // ["reading", "hiking", "painting"]
} catch {
  print(error)
}
```

# טיפול מקיף
במסמכי JSON ניתן להשתמש גם במבנה נתונים מורכב יותר, כמו אובייקטים מקוננים או מערכי אובייקטים. כדי להתמודד עם כל מבנה נתונים כתכונה, מתוך כך אתה יכול לעבור על טכנולוגיות נוספות או טכנולוגיות אחרות

אם לך יש בא געגוע לעבוד עם JSON, אתה יכול לרכישת חפטיות מהקוד לעבוד עם JSON וקבלת מידע ממסמכי JSON שונים

#ראה גם
למידת נושא JSON בעומק נוסף יש לבקר בקישור הבא: https://www.swiftbysundell.com/articles/parsing-json-in-swift/