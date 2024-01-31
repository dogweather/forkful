---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:01:06.021960-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשה על פרוטוקול HTTP היא התהליך שבו אנחנו פונים לשרת ברשת כדי לקבל או לשדר מידע. תכניתנים עושים את זה כדי לאפשר תקשורת בין האפליקציה שלהם למידע מרוחק, כמו APIs או שירותים ווב.

## איך לעשות:
בחבילת Foundation של Swift יש את URLSession שתעזור לנו לשלוח בקשות HTTP. הנה קוד דוגמא לבקשת GET:

```Swift
import Foundation

let url = URL(string: "https://api.example.com/data")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Client Error: \(error.localizedDescription)")
        return
    }
    
    guard let httpResponse = response as? HTTPURLResponse,
        (200...299).contains(httpResponse.statusCode) else {
        print("Server Error")
        return
    }
    
    if let mimeType = httpResponse.mimeType, mimeType == "application/json",
        let data = data,
        let dataString = String(data: data, encoding: .utf8) {
        print("Got data: \(dataString)")
    } else {
        print("Invalid Data Format")
    }
}

task.resume()
```

הפלט יהיה המידע שהשרת מחזיר בתור תגובה לבקשה שלך.

## עיון מעמיק:
בשנים הראשונות של האינטרנט, בקשות HTTP היו יותר פשוטות אבל גם פחות יעילות. כיום, יש מערכת איתות מורכבת יותר עם הגדרות כמו Headers או Caching שיודעת להתמודד עם ביקושים רבים ולעבור נתונים בטוחים ומהירים יותר. 

במערכת URLSession של Swift, לדוגמה, יש העדפה לממשקי עבודה (work sessions) המתמודדים עם בקשות אינטרנט בצורה אסינכרונית - כלומר לא גורמים לאפליקציה לחכות כשהם מתבצעות. זה מאפשר למשתמש להמשיך לעבוד עם האפליקציה בלי עיכובים.

קיימות גם ספריות אחרות כמו Alamofire שמפשטות את תהליך הבקשה והתגובה עוד יותר, אבל URLSession מהווה את הפתרון הסטנדרטי שמובנה ישירות בתוך Swift.

## ראה גם:
- [Swift's URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Working with JSON in Swift](https://developer.apple.com/swift/blog/?id=37)
- [HTTP Request Methods - Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Alamofire GitHub Repository](https://github.com/Alamofire/Alamofire)
