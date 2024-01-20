---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מהו שליחת בקשת HTTP ולמה אנחנו עושים זאת?
שליחת בקשת HTTP היא פעולה בה אנו מבקשים משרת לשלוח מידע בחזרה אלינו. זו חלק חשוב מהתקשורת בין האפליקציה שלנו לשרתים, כך שאנחנו יכולים להוריד מידע מרחוק ולעדכן את ממשק המשתמש שלנו.

## איך משלחים בקשת HTTP?
נלמד איך לשלוח בקשת HTTP באמצעות Swift. הנה דוגמה של איך לשלוח בקשת GET:

```Swift
let url = URL(string: "https://api.example.com/data")
let task = URLSession.shared.dataTask(with: url!) {(data, response, error) in
    if let error = error { print("Error: \(error)") }
    else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str!)")
    }
}
task.resume()
```

פלט של הדוגמה:
```
Received data:
{
    "key1": "value1",
    "key2": "value2"
}
```

## צלילה עמוקה יותר
HTTP הוא הפרוטוקול הראשי של האינטרנט מאז שנוצר בשנות ה-90. יישומי Swift לרוב משתמשים בספריית URLSession לביצוע בקשות HTTP, אך ישנן ספריות צד שלישי כמו Alamofire שמציעות אפשרויות נוספות.

שימו לב שבחיבורים לא בטוחים המידע שלנו יכול להיחשף לגורמים פוגעניים. לכן, כאשר אנו שולחים בקשות HTTP, חשוב להשתמש בחיבור מאובטח (HTTPS).

## קישורים למקורות נוספים
במידה ותרצו לדעת עוד, ניתן לחפש מידע באתרים אלו:
1. URLSession API - Apple Developer Documentation: https://developer.apple.com/documentation/foundation/urlsession
2. W3Schools - Learn HTTP: https://www.w3schools.com/whatis/whatis_http.asp
3. Swift HTTP networking with URLSession course: https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started