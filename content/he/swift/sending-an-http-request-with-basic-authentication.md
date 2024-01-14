---
title:                "Swift: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# למה

בדרך כלל, אתה משתמש בפרוטוקול HTTP כדי לקבל מידע מאתרים או שירותים באינטרנט. אם אתה רוצה להגביל את הגישה למידע רק למשתמשים מסוימים, או אם יש לך נתונים רגישים שאתה רוצה להגן עליהם, אתה יכול לשלוח HTTP בקשה עם אימות בסיסי.

# איך לעשות זאת

בקשת HTTP עם אימות בסיסי דורשת שתשלח שם משתמש וסיסמה כחלק מהבקשה. אתה יכול לעשות זאת בפונקציית ```URLRequest``` באמצעות יצירת אובייקט של ```URLCredential``` והוספתו לכותרת ה-Authorization של הבקשה.

כדי להבין את זה בצורה יותר טובה, נראה דוגמאות של בקשות HTTP עם ובלי אימות בסיסי:

```Swift
// בקשת HTTP רגילה ללא אימות
guard let url = URL(string: "https://example.com") else {
    return
}
let request = URLRequest(url: url)
let session = URLSession(configuration: .default)
let dataTask = session.dataTask(with: request) { data, response, error in
    // עיבוד התגובה של השרת
}
dataTask.resume()

// בקשת HTTP עם אימות בסיסי
guard let url = URL(string: "https://example.com") else {
    return
}
let username = "username" // השלמה בשם המשתמש האמיתי
let password = "password" // השלמה בסיסמה האמיתית
let loginString = "\(username):\(password)"
guard let loginData = loginString.data(using: .utf8) else {
    return
}
let base64LoginString = loginData.base64EncodedString()
var request = URLRequest(url: url)
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
let session = URLSession(configuration: .default)
let dataTask = session.dataTask(with: request) { data, response, error in
    // עיבוד התגובה של השרת
}
dataTask.resume()
```

כדי להראות את ההבדל בין הבקשות השניות, ננסה להדפיס את התגובה מהשרת:

```Swift
// בקשת HTTP רגילה ללא אימות
// Output: "unauthorized"

// בקשת HTTP עם אימות בסיסי
// Output: "success"
```

# צלילה עמוקה

בנוסף לאימות בסיסי, ישנם עוד המון אפרוטסים בנושא של שליחת בקשות HTTP. תהליך האימות ע