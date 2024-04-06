---
date: 2024-01-20 17:45:05.291430-07:00
description: "How to: \u05EA\u05D5\u05E6\u05D0\u05D5\u05EA \u05D4\u05D3\u05D5\u05D2\
  \u05DE\u05D4: \u05EA\u05DC\u05D5\u05D9 \u05D1\u05D3\u05E3 \u05D4\u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05E9\u05EA\u05D1\u05D7\u05E8\u05D5 \u05DC\u05D4\u05D5\
  \u05E8\u05D9\u05D3. \u05D6\u05D4 \u05D9\u05D3\u05E4\u05D9\u05E1 \u05D0\u05EA \u05EA\
  \u05D5\u05DB\u05DF \u05D4\u05D3\u05E3 \u05D1\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC\
  ."
lastmod: '2024-04-05T21:53:40.955939-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
weight: 42
---

## How to:
```Swift
import Foundation

let url = URL(string: "http://example.com")!

let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Error: \(error)")
        return
    }
    guard let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) else {
        print("Error: invalid HTTP response code")
        return
    }
    guard let data = data else {
        print("Error: no data")
        return
    }
    if let pageContent = String(data: data, encoding: .utf8) {
        print(pageContent)
    }
}

task.resume()
```
תוצאות הדוגמה: תלוי בדף האינטרנט שתבחרו להוריד. זה ידפיס את תוכן הדף בקונסול.

## Deep Dive:
דאונלודינג של דפי אינטרנט בצורה זו התחיל להיות פופולרי עם הפצת האינטרנט לקהל הרחב בשנות ה-90. בSwift, URLSession הוא הכלי העיקרי לאינטראקציה עם פרוטוקול HTTP. ישנם גם ספריות שלישיות כמו Alamofire שעלולות להפוך את התהליך ליותר פשוט או יעיל יותר, אבל URLSession מספקת מרבית הצרכים לעבורות מידע ברשת. חשוב לשים לב למבנה התגובה ולסטטוס קודים שמחזיר השרת כדי לעבד את התגובה בצורה נכונה.

## See Also:
- [URLSession | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Working with JSON in Swift](https://developer.apple.com/swift/blog/?id=37)
- [Alamofire GitHub page](https://github.com/Alamofire/Alamofire)
