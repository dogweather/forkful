---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט היא תהליך שבו אנו מקבלים מידע מכתובת URL מסוימת. בשפת התכנות Swift, אנחנו עושים זאת על מנת לטפל בנתונים באופן תכניתי, לדוג': לחלץ מידע, לבצע תיקופים ועוד.

## איך לעשות:

```Swift
import Foundation

let url = URL(string: "http://example.com")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```
תוצאה אפשרית של הקוד העובר את המסננים ומקבל את המידע הנדרש:

```Swift
Received data:
<!doctype html>
<html>
<head>
<title>Example Domain</title>
...
</body>
</html>
```

## הצצה לעומק:

בשנות ה-90, הורדת דפים מהאינטרנט הפך למעשה נפוץ יחסית במהלך פיתוח תוכנה. Swift מציעה לנו URLSession, מחלקה שמאפשרת לנו להוריד מידע מהאינטרנט מבלי להתעסק בפרטים ממומשים של שיחות הרשת.

אפשר לתכנת תהליכים דומים בשפות תכנות אחרות. לדוגמה, ב-Java אנו משתמשים ב-HttpURLConnection, וב-Python אנו משתמשים ב-requests או ב-urllib.

היחידה URLSession מנהלת תהליכים באופן אצילי, מה שחוסך מאיתנו את הצורך להבין את כל הפרטים המעמיקים של חיבורי TCP, HTTP ו-SSL.

## ראו גם:

1. [URLSession - Documentation | Apple Developer](https://developer.apple.com/documentation/foundation/urlsession)