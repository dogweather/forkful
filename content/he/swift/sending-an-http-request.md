---
title:                "Swift: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

שליחת בקשת HTTP היא כלי חיוני בכל פרוייקט של פיתוח אפליקציות עם כמעט כל שימוש ברשת. בעזרת הבקשה, אנו יכולים לקבל מידע משרת ולשלוח פעולות למשתמשים אחרים באפליקציה.

## כיצד לעשות זאת

על מנת לשלוח בקשת HTTP בקוד Swift, ניתן להשתמש במחלקת `URLRequest`. לפניכם דוגמא לכיצד ליצור בקשה GET ולשלוח אותה:

```Swift
if let url = URL(string: "https://www.example.com") {
    var request = URLRequest(url: url)
    request.httpMethod = "GET"
    let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
        if let data = data {
            print("Response data: \(data)")
        }
    }
    task.resume()
}
```

כאן אנו משתמשים ב-`URLSession` כדי לבצע את הבקשה ומקבלים את התשובה בפורמט של מערך `Data`.

## חפירה עמוקה

תהליך של שליחת בקשת HTTP מכיל מספר צעדים נוספים, כגון הוספת נתונים לבקשה (POST), שימוש בסוגי נתונים שונים כמו JSON או מתחילת הקוד של הבקשה. כאשר אנו מתכנתים בסביבת Swift, אנו יכולים להיעזר בספריות חיצוניות כגון Alamofire כדי לספק לנו פעולות נוספות ותכונות לניהול הבקשות שלנו.

## ראו גם

- [מדריך לשליחת בקשת HTTP עם Swift](https://www.hackingwithswift.com/articles/113/how-to-send-an-http-request-using-swift)
- [ספריית Alamofire לניהול דרישות של HTTP](https://github.com/Alamofire/Alamofire)