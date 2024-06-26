---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:20.345293-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05DB\u05D3\u05D9 \u05DC\u05E4\u05E8\u05E7 HTML \u05D1Ruby, \u05D4\u05EA\u05E7\
  \u05E0\u05D5 \u05D0\u05EA \u05D4\u05D2'\u05DD 'Nokogiri' \u05E2\u05DD `gem install\
  \ nokogiri`. Nokogiri \u05D4\u05D5\u05D0 \u05DB\u05DE\u05D5 \u05E1\u05DB\u05D9\u05DF\
  \ \u05E9\u05D5\u05D5\u05D9\u05E6\u05E8\u05D9 \u05DC\u05E2\u05D1\u05D5\u05D3\u05D4\
  \ \u05E2\u05DD HTML \u05D5XML \u05D1Ruby. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\
  \u05D0\u2026"
lastmod: '2024-03-13T22:44:40.200524-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05E4\u05E8\u05E7 HTML \u05D1Ruby, \u05D4\u05EA\
  \u05E7\u05E0\u05D5 \u05D0\u05EA \u05D4\u05D2'\u05DD 'Nokogiri' \u05E2\u05DD `gem\
  \ install nokogiri`."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

## איך לעשות זאת:
כדי לפרק HTML בRuby, התקנו את הג'ם 'Nokogiri' עם `gem install nokogiri`. Nokogiri הוא כמו סכין שוויצרי לעבודה עם HTML וXML בRuby. הנה דוגמא מהירה:

```ruby
require 'nokogiri'
require 'open-uri'

# טעינת תוכן HTML מאתר
html_content = URI.open('http://example.com').read

# פירוק הHTML
doc = Nokogiri::HTML(html_content)

# חילוץ הכותרת
title = doc.xpath('//title').text
puts "כותרת הדף היא: #{title}"
```

זה יוציא משהו כמו: `כותרת הדף היא: Example Domain`.

## צלילה עמוקה
בימים הראשונים של Ruby, האפשרויות לפירוק HTML היו מוגבלות. REXML היה מובנה אך איטי. אז הופיע Hpricot, אך הוא דעך. Nokogiri הושק ב-2008, משלב את נוחות השימוש של Hpricot עם המהירות והעוצמה של libxml, ערכת כלים מוכחת ל-XM״L.

בעולם הפירוק, תמיד יש חלופות. חלק מעדיפים את ספריית 'rexml' המובנית או 'oga', פרסר נוסף לXML/HTML עבור Ruby. אך Nokogiri נשאר האהוב בשל חוסנו ומהירותו, שלא לדבר על מערך התכונות העצום שלו.

מאחורי הקלעים, Nokogiri הופך את הHTML למודל אובייקט של מסמך (DOM)—מבנה עץ. זה מקל על הניווט ושינוי האלמנטים. באמצעות XPath ובוררי CSS, ניתן לזהות כל חלק של מידע שנדרש.

## ראה גם
- הג'ם Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- תיעוד rexml של Ruby: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- הפרסר החלופי 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- למידה על XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
