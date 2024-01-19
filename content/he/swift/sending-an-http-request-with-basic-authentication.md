---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אוטנטיקציה בסיסית היא דרך שבה אתה יכול לאמת את המשתמש בצד שרת. זה שימושי כאשר אתה כותב תוכנית שמתקשרת עם שרת שדורש אימות משתמש.

## איך להשתמש:
אם אנחנו רוצים לשלוח בקשת HTTP עם אוטנטיקציה בסיסית בשפה Swift, הדרך לעשות זאת היא באמצעות URLSession:
```Swift
import Foundation

let username = "username"
let password = "password"

let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

let url = URL(string: "https://example.com")!
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("\(error)")
    } else if let data = data {
        print("Data:\n\(data)")
    }
}
task.resume()
```
## עומק
אימות בסיסי בHTTP הוא לא טכניקה חדשה, והתחיל להתפתח בתחילת שימוש האינטרנט. עם זאת, זה מאוד גמיש ומשמש עדיין באפליקציות רבות. ישנן חלופות, כמו אימות טוקן, אך אלה דורשים הרבה יותר יודעת רקע. קוד Swift שלנו ממיר את שם המשתמש והסיסמה לקידוד בסיסי-64, עובר Lunix "נקה" חומרים לא חוקיים.

## ראה גם
- [RFC-2617](https://tools.ietf.org/html/rfc2617) - מפרט HTTP Authentication.
- [מדריכי Apple](https://developer.apple.com/documentation/foundation/urlsession) - URLSession ואימות בסיסי.
- [אימות ב- HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) - MDN דף המדריך.