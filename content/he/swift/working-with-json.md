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

## למה

לכתוב קוד בשפת Swift היא כיף גדול, אבל לעיתים קורות שנתקלים בצורך לעבוד עם מידע מבני JSON. JSON היא שפת תיאור אובייקט פשוטה וקלה ללמידה. זה משמש לאחסון והעברת מידע במבנה טקסט.

## איך לעשות זאת

כדי להתחיל לעבוד עם JSON בשפת Swift, יש לעקוב אחר השלבים הבאים:

1. התקן את הספרייה `Foundation` בקוד שלך.
2. השתמש בפונקציות `JSONSerialization` ו- `data(withJSONObject:)` כדי להמיר מאינטגרים ל-JSON ולהפוך חזרה.
3. השתמש במבנה נתונים כדי לגשת לערכים ב- JSON.
4. השתמש בתנאים לבדיקה של שדות ב- JSON.
5. השתמש בלולאות לעבור על ערכים רבים ב- JSON.

בהמשך יש לך כמה דוגמאות של קוד ופלט.

### דוגמאות בקוד:

שנה את ערך "name" לשם חדש בקובץ JSON:

```Swift
var json = """
{
    "name": "John",
    "age": 30,
    "hobbies": ["reading", "painting", "cooking"]
}
""".data(using: .utf8)!

do {
    guard var user = try JSONSerialization.jsonObject(with: json, options: []) as? [String: Any] else { return }
    user["name"] = "Sarah"

    let newData = try JSONSerialization.data(withJSONObject: user, options: .prettyPrinted)
    let jsonString = String(data: newData, encoding: .utf8)
    print(jsonString!)
} catch {
    print(error.localizedDescription)
}
```

פלט:

```json
{
  "name" : "Sarah",
  "hobbies" : [
    "reading",
    "painting",
    "cooking"
  ],
  "age" : 30
}
```

בדוק האם יש פעילויות "reading" ברשימת התחביבים בקובץ JSON:

```Swift
var json = """
{
    "name": "John",
    "age": 30,
    "hobbies": ["reading", "painting", "cooking"]
}
""".data(using: .utf8)!

do {
    guard let user = try JSONSerialization.jsonObject(with: json, options: []) as? [String: Any] else { return }
    if let hobbies = user["hobbies"] as? [String], hobbies.contains("reading") {
        print("John loves to read!")
    }
} catch {
    print(error.localizedDescription)
}
```

פלט:

```
John loves to read!
```

### Deep Dive

בנוסף לדוגמאות הקוד, ישנם כמה דברים שחשוב לדעת לגבי עבודה עם JSON בשפת Swift:

- שפת Swift מוצםת בצורה אוטומטית כאשר היא נ