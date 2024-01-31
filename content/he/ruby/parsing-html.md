---
title:                "ניתוח HTML"
date:                  2024-01-20T15:34:11.437306-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"

category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?
פירסופ של HTML הוא התהליך שבו אנחנו לוקחים מסמך HTML וממירים אותו למבנה נתונים שאפשר לעבוד איתו ב-Ruby. אנחנו עושים את זה כדי לחלץ מידע, לשנות תוכן, או לפרסם אותו בצורות שונות.

## איך לעשות:
```Ruby
require 'nokogiri'
require 'open-uri'

# פתח את ה-HTML מURL או קובץ מקומי
html = open('https://example.com/')
doc = Nokogiri::HTML(html)

# חלץ תוויות כותרת
titles = doc.css('h1').map { |node| node.text }
puts titles

# שנה טקסט בפסקה והדפס את ה-HTML המעודכן
doc.css('p').first.content = 'שלום עולם!'
puts doc.to_html
```
פלט דוגמא:
```
["כותרת בדף הדוגמא"]
<!DOCTYPE html>
<html>
  <body>
    <h1>כותרת בדף הדוגמא</h1>
    <p>שלום עולם!</p>
  </body>
</html>
```

## צלילה לעומק
פירסופ של HTML התפתח כשהאינטרנט התפשט. ספריות נפוצות כמו Nokogiri ב-Ruby, BeautifulSoup ב-Python, ו-jsoup ב-Java, הפכו לסטנדרט לעיבוד HTML. פעולה זו חוסכת זמן ומאפשרת אוטומציה של פעולות על מסמכים ברשת.
Nokogiri, למשל, משתמש במנוע XPath ו-CSS selectors לחיפוש יעיל במסמך. זה מתבסס על libxml2 ו-libxslt אשר מספקים יכולות נרחבות לעיבוד XML ו-XSLT, גם כן.

אלטרנטיבות ל-Nokogiri כוללות ספריות כמו Oga או Hpricot (שכיום כבר לא בשימוש רב). כל ספרייה מציעה נקודות חוזק וחולשה שווה להתייחס. לעיתים הבחירה בספרייה תלויה בצרכים הספציפיים של הפרויקט או בהעדפות האישיות של המפתח.

## ראה גם
- [Nokogiri מדריך רשמי](http://www.nokogiri.org/)
- [מידע נוסף על libxml2](http://xmlsoft.org/)
- [מדריכים לניתוח HTML ב-Ruby](https://www.ruby-toolbox.com/categories/html_parsing)
