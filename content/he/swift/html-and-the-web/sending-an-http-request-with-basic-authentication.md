---
date: 2024-01-20 18:03:00.717903-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05E7\u05D5\
  \u05D3 \u05D1-Swift \u05E9\u05E9\u05D5\u05DC\u05D7 \u05D1\u05E7\u05E9\u05EA HTTP\
  \ \u05E2\u05DD \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9."
lastmod: '2024-04-05T21:53:40.957026-06:00'
model: gpt-4-1106-preview
summary: "(\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) \u05E7\u05D5\u05D3\
  \ \u05D1-Swift \u05E9\u05E9\u05D5\u05DC\u05D7 \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\
  \u05DD \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

## How to: (איך לעשות:)
קוד ב-Swift ששולח בקשת HTTP עם אימות בסיסי:

```Swift
import Foundation

let username = "your_username"
let password = "your_password"
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

if let url = URL(string: "https://example.com/protected") {
    var request = URLRequest(url: url)
    request.httpMethod = "GET"
    request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        guard let data = data, error == nil else {
            print(error?.localizedDescription ?? "No data")
            return
        }
        if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 {
            print("Authenticated and received data:", String(data: data, encoding: .utf8) ?? "")
        } else {
            print("Authentication failed")
        }
    }
    task.resume()
}
```
תוצאת דוגמה:
```
Authenticated and received data: {"message":"Hello, secure world!"}
```
או:
```
Authentication failed
```

## Deep Dive (עומק הנושא)
ב-Hypertext Transfer Protocol (HTTP), אימות בסיסי הוא שיטה פשוטה להעברת שם משתמש וסיסמה. ה-MIME base64 קודד את הזוגות שם משתמש-סיסמא. האימות הבסיסי קל ליישם ולכן היה פופולרי, אך גם נחשף לפישינג וגניבת זהות.

החלפות כוללות אימות דיגיטלי, עוגיות מאובטחות, או אימות OAuth.

מימוש יכול לכלול גם מעקב לוג או להיות חלק ממנגנון אבטחה רחב יותר באפליקציה. שקול להשתמש URLSesssionDelegate עבור ניהול מתקדם של תהליך האימות.

## See Also (ראה גם)
- [HTTP Authentication: Basic and Digest Access Authentication](https://datatracker.ietf.org/doc/html/rfc7617) - המפרט של ה-RFC לאימות בסיסי.
- [NSURLSession Class - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsurlsession) - המידע הרשמי על השימוש ב-NSURLSession ב-Swift.
- [URLSession Authentication with Swift](https://www.raywenderlich.com/110458/nsurlsession-tutorial-getting-started) - מדריך לתחילת עבודה עם NSURLSession ואימות ב-Swift.
