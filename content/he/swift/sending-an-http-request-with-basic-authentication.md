---
title:                "שליחת בקשת Http עם אימות בסיסי"
html_title:           "Swift: שליחת בקשת Http עם אימות בסיסי"
simple_title:         "שליחת בקשת Http עם אימות בסיסי"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

<h2>למה:</h2>
נשתמש יותר מתוך שתי משפטים כדי להסביר <b>למה</b> מישהו ירצה לשלוח בקשת HTTP עם אימות בסיסי.

<h2>איך לעשות:</h2>
לדוגמאות של קוד ופלט תוצאה נשתמש בבלוקים של "```Swift … ```".

קוד לשליחת בקשה HTTP עם אימות בסיסי:
```Swift
let url = URL(string: "https://example.com/api")
let username = "user"
let password = "password"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
guard let base64LoginString = loginData?.base64EncodedString() else { return }
let request = URLRequest(url: url)
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error.localizedDescription)")
        return
    }
    if let data = data {
        // התגובה מבוצעת כאן
        print("Response: \(String(describing: data))")
    }
}
task.resume()
```

תוצאת הפלט של שאילתת ה-HTTP תכיל את החבילה מהשרת תחת `data` משתנה.

<h2>עיון עמוק:</h2>
עכשיו שיש לנו קצת יותר מידע אודות שליחת בקשה HTTP עם אימות בסיסי, נעמיק קצת יותר.

מהי בקשת HTTP?
בקשת HTTP היא צורת יצירת קשר בין הלקוח (הפלטפורמה הישבתת) לבין השרת (השרת המארח את האפליקציה). זהו דרך מקובלת לקבל נתונים מהשרת או לשלוח נתונים אליו.

מהו אימות בסיסי?
אימות בסיסי הוא תהליך שבו משתמש נדרש להזין שם משתמש וסיסמה כדי לקבל גישה לשירות או משאב מסוים בשרת. זהו דרך נפוצה לבצע אימות על מנת להבטיח שרק משתמשים מורשים יוכלו לגשת למרחב המוגן.

<h2>ראה גם:</h2>
- [תיעוד למחלקת URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [מאמר בנושא אימות בסיסי ב-Swift](https://medium.com/@abhimuralidharan/http-basic-authentication-in-swift-c0976ea92f91)