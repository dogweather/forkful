---
title:                "פירוק HTML"
html_title:           "Swift: פירוק HTML"
simple_title:         "פירוק HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/parsing-html.md"
---

{{< edit_this_page >}}

## למה
ניתן לחצות HTML כדי לקבל תוכן מהאינטרנט לשימוש פנימי באפליקציות שלכם. זה יכול להיות שימושי להציג מידע או לבצע פעולות מסוימות באופן אוטומטי.

## איך לעשות זאת
כדי לקבל תוכן מהאינטרנט, ניתן להשתמש בפקודה DataTask כדי לשלוח בקשת HTTP ולקבל מענה. לאחר מכן, ניתן להתמקד בחלק התגובה הרלוונטי ולחלץ ממנו את התוכן הרצוי באמצעות פירוק HTML. לדוגמה:

```swift 
if let url = URL(string: "https://www.example.com") {
    let task = URLSession.shared.dataTask(with: url, completionHandler: { (data, response, error) in
        if let data = data {
            if let htmlString = String(data: data, encoding: .utf8) {
                let parsedHtml = self.parseHTMLString(htmlString: htmlString)
		print(parsedHtml)
            }
        }
    })
    task.resume()
}

func parseHTMLString(htmlString: String) -> String? {
    if let doc = try? HTML(htmlString: htmlString, encoding: .utf8) {
        let content = doc.css("div.content").first
        return content?.text
    }
    return nil
}
```

כפי שאתם רואים בדוגמה, אנו שולחים בקשת HTTP לכתובת האתר המבוקשת ומחזירים את התוכן הנמצא בתווית div עם ה- class "content" בתוך התגובה.

## חקירה מעמיקה
היתרונות של פירוק HTML כוללים את היכולת להציג מידע מותאם אישית ולשלבו בתוך אפליקציה בצורה מתאימה לעיצוב האפליקציה. כמו כן, זה יכול להיות שימושי גם לפעולות אוטומטיות כגון מילוי טפסים או חיפוש מידע באתרים מרובים.