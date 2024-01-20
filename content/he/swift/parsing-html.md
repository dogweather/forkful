---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/swift/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח HTML הוא תהליך של הפרדת קובץ HTML לרכיביה המרכזיים - תגים, טקסט, תמונות וכו'. מתכנתים עושים את זה כאשר הם זקוקים לשלוט ולתפעל עם נתונים מדף אינטרנט מסוים.

## איך:
להלן דוגמה לקוד שמנתח HTML באמצעות Swift, ואז מציג מספר מעוצב של קישורים:

```Swift
import SwiftSoup

func parseHtml() throws {
    let htmlContent = "<html><body><a href='www.example1.com'>Example1</a><a href='www.example2.com'>Example2</a></body></html>"
    let doc: Document = try SwiftSoup.parse(htmlContent)
    let links: Elements = try doc.select("a")

    for link in links.array() {
        let linkHref = try link.attr("href")
        let linkText = try link.text()
        print("Text: \(linkText), URL: \(linkHref)")
    }
}

do {
    try parseHtml()
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("error")
}
```

## צלילה עמוקה
ביצועים של ניתוח HTML הם בעיה בלתי ברורה בביצועי מנות החיפוש שלנו. ההיסטוריה שמאחורי כלי הניתוח הם טמונים בבעיות של שפות HTML צעירות ודינמיות מאוד, לרבות הבנה של כיצד להתמודד עם שפת סימון מורכבת וללא תוקף כמו HTML. דינמיות הניתוח הוקפאה כאשר HTML5 הגיע, וכעת אנו בראשית שיפור הביצועים של מנות החיפוש.

בנוסף ל-SwiftSoup, שהוא מעולה לניתוח HTML, ישנם כלים נוספים כדי לעבוד עם HTML ב-Swift, כולל HTMLKit ו-Fuzi.

מנגנון מרכזי שמאחורי SwiftSoup הוא JSoup, מנות החיפוש העצמאית של Java לרכיבים של HTML, כאשר היא כתובה ב- Java.

## ראה גם
לקבלת מידע נוסף בנושא, הסתכל מן [Documents of SwiftSoup](https://www.scrapehero.com/how-to-parse-html-in-swift/), [Introduction to HTMLKit](https://github.com/vapor/html-kit), [Introduction to Fuzi](https://github.com/cezheng/Fuzi)