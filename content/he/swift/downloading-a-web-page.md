---
title:                "הורדת עמוד אינטרנט"
html_title:           "Swift: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
ההורדה של עמוד אינטרנט היא פעולת מחשב המאפשרת למתכנתים לקבל את תוכן עמוד אינטרנט כדי לעבוד עם המידע הנמצא בו. ההורדה של אתר אינטרנט מועילה לתוצאות חיפוש, אפליקציות ופרויקטים שונים.

## כיצד לעשות זאת:
כדי להוריד דף אינטרנט בSwift, ניתן להשתמש בפונקציה בשם "URLSession" יחד עם פקודות נוספות כמו "URL", "Data" ו- "Task" כדי ליצור את הבקשה ולקבל את התוכן. למשל, ניתן להשתמש בפונקציה להוציא את תוכן העמוד ולהדפיס אותו:

```Swift
guard let url = URL(string: "https://www.example.com") else { return }
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    guard let data = data, error == nil else { return }
    print(String(data: data, encoding: .utf8))
}
task.resume()
```

## נכים בעומק:
בעבר, כדי להוריד עמוד אינטרנט, היה על המתכנת להשתמש בתוכניות חיצוניות נפרדות או לבנות תוכנה מגבלתית עם דרישות מיוחדות. עכשיו, עם העובדות מתחת לשטיח, תכניות נפלאות כמו פונקצית "URLSession" זמינות לחברת הקוד הפתוח כדי לאפשר הורדה קלה של תוכן אינטרנט ישירות מתוך קוד פרויקט.

## ראו גם:
למידע נוסף על דרכים נוספות לעבוד עם דפי אינטרנט בשפת Swift, ניתן לצפות במדריכים הבאים ברשת:

- [איך להוריד תמונה מאינטרנט בשפת Swift](https://www.hackingwithswift.com/example-code/networking/how-to-download-an-image-from-the-web) 
- [איך לשלב REST API לפרויקט Swift](https://www.raywenderlich.com/18-afnetworking-2-0-tutorial)
- [פרויקט פתוח ומתוחזק עם דוגמת כריית דפי אינטרנט](https://github.com/Alamofire/Alamofire)