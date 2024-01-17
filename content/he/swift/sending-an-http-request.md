---
title:                "שליחת בקשת http"
html_title:           "Swift: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

שלום לקוראי הבלוג הנושם,
בימים שבהם מפתחי תוכנה אינם מספיקים לכתוב קוד קשיח ומורכב, נדרש מהם גם להתמודד עם התקשורת עם שרתים ואתרי אינטרנט. כאן נכנסת לתמונה פעולת השליחת בקשות HTTP, שיטה שמאפשרת לסדר את התקשורת הנדרשת בין התוכנה לשרתים חיצוניים.

## מה ולמה?
שליחת בקשות HTTP היא פעולה דרושה לכתיבת תוכנה המשתמשת בתקשורת בין התוכנה לשרתים חיצוניים. זה עשוי להיות משהו כפשוט כמו קבלת עדכונים מתוך אתר אינטרנט או משהו מורכב יותר כמו פעולות בניה ועדכון של מאגרי נתונים.

## איך לעשות זאת?
כדי לשלוח בקשות HTTP בתוך קוד Swift ניתן להשתמש בפרוטוקול HTTP שהוכנס לכלי העבודה של אפל - Xcode. עבור מחברת אינטרנט כמו פייסבוק, אפשר פשוט ליצור בקשה GET לכתובת URL של פוסט ולקבל את התוצאה הנדרשת. לדוגמה:

```Swift
let postUrl = "https://www.facebook.com/1234567890/posts"
let request = URLRequest(url: postUrl)
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
  if let data = data {
    // התוכן של התגובות בפוסט
    print(String(data: data, encoding: .utf8)!)
  }
}
// ביצוע הבקשה
task.resume()
```

## כיורד
מקור נוסף למידע על השתלחת בקשות HTTP הוא מאמרים נוספים באינטרנט, וכמובן תיעוד רשמי מאת אפל עצמה המסביר איך לשלוח בקשות באמצעות פרוטוקול HTTPS.

## מקורות נוספים
מאמרים ומדריכים נוספים מומלצים:
- [מאמר מאת אפל עצמה](https://developer.apple.com/documentation/foundation/urlsession)
- [מאמר בעברית בנושא שליחת בקשות HTTP בתוך קוד Swift](https://www.webmaster.org.il/dev/introduction-to-http-request-in-swift/)