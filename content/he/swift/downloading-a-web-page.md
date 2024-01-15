---
title:                "להורדת דף אינטרנט"
html_title:           "Swift: להורדת דף אינטרנט"
simple_title:         "להורדת דף אינטרנט"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

אנשים ירצו להוריד דף אינטרנט מכמה סיבות: לצורך גילוי אבטחה, לצורך נתונים מידעיים או פשוט כי הם נקלעו לקישור מעניין. באמצעות קוד Swift יכולים להעניק לעצמם את הכלים להוריד דף אינטרנט, לעבד את התוצאות ולהתאים אותם לצרכים שלהם.

## איך לעשות

הכירו את הנציג טיפוס URL של Swift. יש ליצור אובייקט זה כדי ליצור קישור לכתובת האינטרנט שתרצו להוריד. לכלול את הקוד הבא בתוך אובייקט URL כדי לייצר בקשת HTTP GET:

```Swift
let url = URL(string: "https://www.example.com")!
```

כעת ניתן להשתמש במחלקה URLSession כדי לבצע את הבקשה ולקבל את התוכן המלא של הדף:

```Swift
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let data = data, let string = String(data: data, encoding: .utf8) {
        print(string)
    }
}
task.resume()
```

לאחר מכן, ניתן לבצע עיבוד נוסף על התוצאות לפי הצורך.

## מעמקים

דפי אינטרנט ניתנים להורדה באמצעות טכניקות שונות, כגון HTTP and HTTPS. בנוסף, ניתן גם להשתמש בקוד מקור קיים כדי לממש בקשות GET, POST וכו' בפשטות. כמו כן, ניתן להתאים ולהתאים את הקוד הזה לצורכים שונים כגון הורדה של תמונות או קבצים.

## ראו גם

- ["איך לממש בקשות GET וPOST בקוד Swift"](https://www.hackingwithswift.com/example-code/system/how-to-send-the-contents-of-a-url-to-the-server-using-post) 
- ["הורדת תמונות באמצעות Swift"](https://www.appcoda.com/download-images-rest-api/) 
- ["מדריך מקיף לתכנות בשפת Swift"](https://www.apple.com/swift/)