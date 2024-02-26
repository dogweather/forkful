---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:55.239122-07:00
description: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05DE\u05EA\u05D9\u05D9\u05D7\u05E1\
  \ \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E4\u05D9\u05E8\u05D5\u05E7\
  \ \u05D5\u05E4\u05E8\u05E9\u05E0\u05D5\u05EA \u05E9\u05DC \u05DE\u05D1\u05E0\u05D4\
  \ \u05EA\u05D5\u05DB\u05DF HTML, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05D1\
  \u05DB\u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E9\u05E0\
  \u05D5\u05EA \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05D4\u05D6\u05D4 \u05D1\
  \u05D0\u05D5\u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9. \u05EA\u05DB\u05E0\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E1\u05E7\u05D9\u05DD \u05D1\u05E4\u05E2\
  \u05E0\u05D5\u05D7\u2026"
lastmod: '2024-02-25T18:49:38.142944-07:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05E2\u05E0\u05D5\u05D7 HTML \u05DE\u05EA\u05D9\u05D9\u05D7\u05E1\
  \ \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E4\u05D9\u05E8\u05D5\u05E7\
  \ \u05D5\u05E4\u05E8\u05E9\u05E0\u05D5\u05EA \u05E9\u05DC \u05DE\u05D1\u05E0\u05D4\
  \ \u05EA\u05D5\u05DB\u05DF HTML, \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05D1\
  \u05DB\u05D3\u05D9 \u05DC\u05D7\u05DC\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05E1\u05E4\u05E6\u05D9\u05E4\u05D9\u05D9\u05DD \u05D0\u05D5 \u05DC\u05E9\u05E0\
  \u05D5\u05EA \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05D4\u05D6\u05D4 \u05D1\
  \u05D0\u05D5\u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9. \u05EA\u05DB\u05E0\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E1\u05E7\u05D9\u05DD \u05D1\u05E4\u05E2\
  \u05E0\u05D5\u05D7\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח HTML מתייחס לתהליך של פירוק ופרשנות של מבנה תוכן HTML, בדרך כלל בכדי לחלץ נתונים ספציפיים או לשנות את התוכן הזה באופן תכנותי. תכנתנים עוסקים בפענוח HTML עבור גריפת אתרים, חציבת נתונים, בדיקות אוטומטיות, ומשימות של העברת תוכן, מה שמאפשר ליישומים לתקשר עם מסמכי רשת ולעבד אותם ביעילות.

## איך לעשות:
Swift, כברירת מחדל, אינה כוללת ספרייה מובנית לפענוח HTML, דבר המחייב שימוש בספריות צד שלישי כדי לבצע את המשימה הזו בצורה יעילה. אחת האפשרויות הפופולריות ביותר היא SwiftSoup, ספרייה טהורה של Swift שמציעה תחביר דומה ל-jQuery לפענוח ולשינוי HTML.

### התקנה
ראשית, אתה צריך להוסיף את SwiftSoup לפרוייקט שלך. אם אתה משתמש במנהל החבילות של Swift, תוכל להוסיף אותו לתלות שלך ב`Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
]
```

### דוגמה: חילוץ קישורים מ-HTML
נניח שיש לך מסמך HTML ואתה רוצה לחלץ את כל הקישורים (`<a href="...">`). עם SwiftSoup, אתה יכול לעשות זאת בקלות:

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>דף דוגמה</title>
</head>
<body>
    <p>ברוכים הבאים לאתר שלנו</p>
    <a href="https://example.com/page1">עמוד 1</a>
    <a href="https://example.com/page2">עמוד 2</a>
</body>
</html>
"""

do {
    let doc: Document = try SwiftSoup.parse(html)
    let links: Elements = try doc.select("a")
    for link in links.array() {
        let linkHref: String = try link.attr("href")
        let linkText: String = try link.text()
        print("\(linkText) - \(linkHref)")
    }
} catch Exception.Error(let type, let message) {
    print("סוג השגיאה: \(type) הודעה: \(message)")
} catch {
    print("שגיאה")
}
```

### פלט לדוגמה
הקוד הקודם מחלץ URLs ואת טקסטם מה-HTML, ומוציא לפלט:

```
עמוד 1 - https://example.com/page1
עמוד 2 - https://example.com/page2
```

הדוגמה הבסיסית הזו מדגימה איך לנצל את SwiftSoup לצורך פענוח מסמכי HTML. בחקירה נוספת של תיעוד SwiftSoup, תוכל למצוא שיטות רבות נוספות לנווט, לחפש, ולשנות את תוכן ה-HTML, דבר המעניק ליישומי Swift שלך את היכולת לעבד תוכן רשת מורכב בקלות.
