---
title:                "ניתוח HTML"
date:                  2024-01-20T15:34:06.809775-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירוס HTML הוא תהליך של הפיכת מבנה HTML למבנה נתונים שניתן לניתוח ושימוש בקוד. תוכניתנים עושים זאת כדי לקרוא או לשנות את תוכן ומבנה דפי אינטרנט באופן אוטומטי.

## איך לעשות:
בדוגמה נשתמש בספריית `SwiftSoup`. קודם כל, צריך להתקין את הספרייה דרך Swift Package Manager.

```Swift
import SwiftSoup

let htmlString = "<html><head><title>שלום</title></head><body><p>זה טקסט בעברית!</p></body></html>"

do {
    let doc: Document = try SwiftSoup.parse(htmlString)
    let bodyText = try doc.body()?.text()
    print(bodyText ?? "לא נמצא טקסט")
} catch Exception.Error(let type, let message) {
    print("משהו השתבש: \(type) \(message)")
} catch {
    print("אירעה שגיאה כללית")
}
```

פלט דוגמה:
```
זה טקסט בעברית!
```

## ניתוח עמוק
הרעיון שמאחורי ניתוח HTML הוא לא חדש. כבר בשנות ה-90 התוכניתנים נאלצו לעבוד עם HTML כדי לטעון ולעבד נתונים מדפי אינטרנט. הבעיה המרכזית היא ש-HTML נוטה לא להיות מהוקצע ולכן זקוק לניתוח קפדני. בעולם המודרני, ספריות כמו `SwiftSoup` מנצלות כלים לניתוח מורכב כדי להסיר את הקושי הזה. ישנן גם אלטרנטיבות כמו `WebKit` או `libxml2` עבור מטרות שונות, כולל פיר(מ)וס של קבצים XML.

## ראה גם
- [SwiftSoup GitHub](https://github.com/scinfu/SwiftSoup)
- [WebKit Documentation](https://developer.apple.com/documentation/webkit)
- [libxml2](http://xmlsoft.org/)