---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:45:05.291430-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה הורדת דף אינטרנט? זה תהליך שבו אנחנו מקבלים את תוכן דף האינטרנט בתוכנית שלנו. למה? כדי לעבד מידע, להציגו באפליקציה או לקחת ממנו נתונים.

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